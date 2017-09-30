/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Try}

sealed abstract class Remember // signals remember values; event-streams do not
case object SignalKind extends Remember
case object StreamKind extends Remember

/** A node in the directed graph of reactives. */
private[drx] class InternalRx[X] private[drx]
  (private[drx] val remember: Remember, name: String)
{
  // private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )
  val id: String = "" + internals.count + name
  private[drx] def underlying: InternalRx[X] = this
  private[drx] var debugEvaluating: Boolean = false
  debug.debugRxs(this) = Unit // for debugging

  // signals remember the last value or an exception encoutered during evaluation
  private[drx] var value: Try[X] = internals.TheEmptyStream
  // to calculate the next value
  private[drx] var formula: ()=>X = () => value.get
  // for propagation, push to these rx
  private[drx] val outs: mutable.Set[InternalRx[_]] = mutable.Set()
  // for dynamic edges, remember incomings
  private[drx] val ins: mutable.Set[InternalRx[_]] = mutable.Set()
  // turn unobserved into unreachable, if true, force evaluation
  private[drx] var forceActive: Boolean = false
  // turn frozen into unreachable, if true, .value will never change
  private[drx] var isFrozen: Boolean = false
  // for glitchfreedom, always higher than the all incomings level
  private[drx] var level: Int = 0
  // for optimizations on static edges, static incomings
  private[drx] var father: Option[InternalRx[_]] = None

  // INVARIANT: noturn ==> (!isEvent <==> value != None)
  // INVARIANT: outs         .forall(out => out.level > this.level)
  // INVARIANT: (ins++father).forall(in  => in.level  < this.level)
  // INVARIANT: !this.isNeeded ==> debugRxs.forall( rx => !rx.out.contains(this) )

  private[drx] def getIns: Set[InternalRx[_]] = ins.toSet ++ father
  private[drx] def getOuts: Set[InternalRx[_]] = outs.toSet
  private[drx] def isActive: Boolean = outs.nonEmpty || forceActive

  private[drx] def tryGetValue(outer: Option[InternalRx[_]]): Try[X] = withTransaction { tx =>
    // if we are frozen, we return our immutable value
    if (isFrozen) return value

    // if we are inactive, our value may be invalid, so we mark us for reevaluation
    if (!isActive && !withTransaction(_.clean.contains(this))) tx.markRx(this)

    outer.foreach { outer =>
      // if outer exists and isActive, then we are in push-mode (propagation)
      // and need to setup connection for continued pushing
      if (outer.isActive) connect(outer)

      // if outer exists, we need to ensure that our level is lower than the
      // outer level.
      ensureLowerLevel(this, outer)

      // it is imporant to connect before changing levels, because
      // ensureLowerLevel has to change the level of connected reactives, too
    }

    // return valid value
    tx.tryGetValidValue(this, outer)
  }

  private[drx] def reeval(): Unit = {
    debugEvaluating = true
    debug.writeToDisk()

    // during evaluation of formula, the 'ins' will be filled with our dependencies.
    val tmpIn = Set() ++ ins; ins.clear() // clear ins, as they will be readded through the next line
    val newValue = internals.activeRx.withValue(Some(this)) { Try(formula()) }
    (tmpIn -- ins).foreach(_.unpushto(this)) // stop pushing from dependencies, we do not depend on anymore

    // If the newValue does not signify to Abort evaluation (EmptyStream) and
    // the value actually changed, compared to the last value.
    val failedWithEmptyStream = newValue.failed.map(_.isInstanceOf[EmptyStream]).getOrElse(false)
    if (!failedWithEmptyStream && value != newValue) withTransaction { tx =>
      // save new value
      value = newValue

      // if we are currently pushing, we mark all outgoings for reevaluation, too.
      outs.foreach(tx.markRx) // if (mode == PushMode)

      // if the current reactive is a stream, we have to forget our value
      // after the transaction, by setting it to Abort.
      if (remember == StreamKind) tx.runLater(() => value = internals.TheEmptyStream )
    }

    withTransaction.activeCtx.value.map(_.clean += this)

    //// TODO
    // if (ins.isEmpty) this.kill()

    debug.writeToDisk()
    debugEvaluating = false
  }

  private[drx] def unpushto(to: InternalRx[_]): Unit = {
    // if (!outs.contains(to)) throw new RuntimeException(s"redundant broke from $id to ${to.id}") else
    // println(s"  broke from $id to ${to.id}")
    outs -= to
    // if we just lost our last outgoing, then we deactivate ourself, too.
    if (!isActive) (ins ++ father).foreach(_.unpushto(this))
  }

  private[drx] def freeze(): Unit = {
    isFrozen = true
    outs.foreach { it => if ((it.ins ++ it.father).forall(_.isFrozen)) it.freeze() }
    outs.clear()
    formula = () => value.get
    father = None
    ins.clear()
  }

  private def connect(outer: InternalRx[_]) = {
    this.outs += outer // for propagation,   remember outgoing rx
    outer.ins += this  // for dynamic edges, remember incoming rx
  }

  private def ensureLowerLevel(from: InternalRx[X], to: InternalRx[_]): Unit = {
    /** there are two kinds of strategies to handle this situation,
      * you can choose between over the global variable [[internals.withRetries]]. */
    if (internals.withRetries) to.levelup(from.level + 1)
    else                     from.leveldown(to.level - 1)
  }

  private def levelup(newLevel: Int): Unit = if (newLevel > level) {
    level = newLevel
    withTransaction(_.resubmit(this))
    outs.foreach(_.levelup(newLevel + 1))
  }

  private def leveldown(newLevel: Int): Unit = if (newLevel < this.level) {
    level = newLevel
    withTransaction(_.resubmit(this))
    (ins ++ father).foreach(_.leveldown(newLevel - 1))
  }

  override protected def finalize(): Unit =
    println("fin " + this.id)
}

class EmptyStream(cause: Throwable) extends Throwable(cause)

private object internals {
  val withRetries = false // true = old variant, false = cooler variant

  val TheEmptyStream = Failure(new EmptyStream(null))

  val activeRx: DynamicVariable[Option[InternalRx[_]]] = new DynamicVariable(None)

  var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
}

//  val strongEventsources: mutable.Set[InternalRx[_]] = mutable.Set()
//  val dummyOwner = new VarOwner {}
//  def checksafety(me: InternalRx[_], owner: VarLike): Unit = {
//    val ancestry: mutable.Set[InternalRx[_]] = mutable.Set()
//    def transitiveIns(it: InternalRx[_]): Unit = {
//      if (ancestry.add(it)) it.getIns.foreach(transitiveIns)
//    }
//    transitiveIns(me)
//    val vars = ancestry.collect { case it: EventSource[_] => it }
//    if (vars.size > 1 && !vars.subsetOf(owner.getEventsources))
//      throw new RuntimeException("You are creating a fold over multiple variables." +
//        " The fold will live until all incoming variables are dead.." +
//        " You must specify the VarOwner of all Variables that influence this fold." +
//        " for example like: a = owner.mkVar(); b = owner.mkVar() _.fold(0, owner)(_+_) ." +
//        " The fold will be then be collected when this VarOwner is collected.")
//  }
