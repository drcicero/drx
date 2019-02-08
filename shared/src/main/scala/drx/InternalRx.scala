/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Try}
import java.util.concurrent.ThreadLocalRandom

/** A node in the directed graph of reactives. */
private[drx] class InternalRx[X] private[drx](name: String) extends Observable[X] {
  // to find sourcefile and line numbers again // hashCode would also give each a different id
  val id: String = if (name=="") "" + ThreadLocalRandom.current().nextInt().toString + "_" else name

  // contains value or exceptions calculated by formula during turns
  private[drx] var value: Try[X] = internals.emptyValExc()
  // contains value or exceptions calculated by formula during turns
  private[drx] var shouldPush: Boolean = false

  // can be overwritten in subclasses to force evaluation
  private[drx] var forceActive: Boolean = false
  // can be overwritten in subclasses to produce the next value
  private[drx] var formula: ()=>X = () => value.get
  // can be overwritten in subclasses to not clear the value between turns
  private[drx] var remember: Boolean = false

  // for propagation, push to these rx
  private[drx] val outs: mutable.Set[InternalRx[_]] = mutable.Set()
  // for dynamic edges, remember incomings
  private[drx] val ins: mutable.Set[InternalRx[_]] = mutable.Set()
  // you cannot trust ins if not run at least once
  private[drx] var runatleastonce: Boolean = false
  // for glitchfreedom, always higher than the all incomings level, and lower than all outgoing levels
  private[drx] var level: Int = 0

  private[drx] def getIns: Set[InternalRx[_]] = ins.toSet
  private[drx] def getOuts: Set[InternalRx[_]] = outs.toSet
  private[drx] def getMode: Mode = {
    if (outs.nonEmpty || forceActive) Pushing
    else if (runatleastonce && getIns.isEmpty && !forceActive) Frozen
    else Paused
  }
  // INVARIANT: outs         .forall(out => out.level > this.level)
  // INVARIANT: getIns       .forall(in  => in.level  < this.level)
  // INVARIANT: mode==Paused ==> forall rx. !rx.out.contains(this)

  // for debug.writeToDisk
  private[drx] var debugEvaluating: Boolean = false // currently evaluating
  debug.debugRxs(this) = Unit
  private[drx] var debugCtr: Int = 0 // how often evaluated

  //  // by giving each Node more memory, we can find memory leaks more easily
  //  private[drx] val dummy = Array.tabulate(1000 * 100)( i => i ) // TODO

  // for proxyvars a pointer to yourself
  private[drx] def underlying: InternalRx[X] = this

  @inline private[drx] def getValue: X = withInstant { tx =>
    println("getting " + id + " (" + value + ")")
    val outer: Option[InternalRx[_]] = internals.activeRx.value

    getMode match {
      // if we are frozen, we return our immutable value
      case Frozen  => println("is frozen: " + this.id); return value.get

      // if we are paused, and we have not yet evaluated (clean),
      // value is invalid and we mark ourself for reevaluation.
      //      case Paused  => if (!tx.clean.contains(this)) tx.markRx(this)

      // everything ok
      //      case Pushing =>

      case _ =>
    }
    if (if (remember) getMode == Paused && !tx.clean.contains(this)
    else value.isFailure && value.failed.get.isInstanceOf[EmptyValExc])
        tx.markRxShallow(this)

    // if outer exists
    outer.foreach { outer =>
      // if outer is pushing (vs paused == pulling), remember to pushto outer on future changes
      if (outer.getMode==Pushing) this pushto outer
      // ensure  this.ins.level < this.level < outer.level < outer.outs.level
      this.setLevelLowerThan(outer, tx)
    }

    // So now, we process any lower or equal signals than this now!
    if (internals.withRetries) {
      val head = tx.dirty.peek
      if (head != null && head.level <= level) {
        // we throw an exception and mark outer to be evaluated later again,
        // the enclosing transaction will then continue evaluation.
        outer.foreach(tx.markRx)
        internals.emptyValExc().get
      }
    } else {
      tx.processLowerOrEqual(level)
    }

    // we have ensured that the value is valid, so we can return it
    value.fold(y => internals.emptyValExc[X](y).get, y => y)
  }

  private[drx] def reeval(): Unit = {
    // during evaluation of formula, the 'ins' will be filled with our dependencies.
    val tmpIn = Set() ++ ins; ins.clear()
    val newValue = internals.activeRx.withValue(Some(this)) { Try(formula()) }
    // stop pushing from dependencies, we do not depend on anymore
    (tmpIn -- ins).foreach(_ unpushto this)

    if (getMode == Pushing) runatleastonce = true
    //internals.activeRx.value.foreach { outer =>
    //  if (outer.getMode == Pushing) runatleastonce = true }

    // If the newValue does not signify to Abort evaluation (EmptyStream).
    println(f"$id%-20s := $newValue%-20s")
    val failedWithEmptyStream = newValue.failed.map(_.isInstanceOf[EmptyValExc]).getOrElse(false)
    if (!failedWithEmptyStream /* && newValue != value */) withInstant { tx =>
      // save new value
      value = newValue
      debugCtr += 1
      // if we are currently pushing, we mark all outgoings for reevaluation, too.
      println(shouldPush)
      if (shouldPush) outs.foreach(tx.markRx)
    }

    withInstant.activeCtx.value.map(_.clean += this)
    //    debug.writeToDisk(s"STEP $id := $value")
    // if (ins.isEmpty) this.kill() //// TODO
  }

  private[drx] def freeze(): Unit = {
    formula = () => value.get
    ins.clear()
    runatleastonce = true

    val tmp = Set() ++ outs; outs.clear()
    tmp.foreach { it => if (it.getIns.forall(_.getMode == Frozen)) it.freeze() }
  }

  private[drx] def unpushto(to: InternalRx[_]): Unit = {
    //if (!outs.contains(to)) throw new RuntimeException(
    //  s"redundant broke from $id to ${to.id}") else
    //println(s"  broke from $id to ${to.id}")
    outs -= to
    // if we just lost our last outgoing, then we deactivate ourself, too.
    if (getMode!=Pushing) getIns.foreach(_ unpushto this)
  }

  private def pushto(outer: InternalRx[_]) = {
    this.outs += outer // for propagation / consistency,              remember outgoing rx
    outer.ins += this  // for dynamic edge removal / time management, remember incoming rx
  }

  private def setLevelLowerThan(to: InternalRx[_], tx: Instant): Unit = {
    /** there are two kinds of strategies to handle this situation,
      * you can choose between over the global variable [[internals.withRetries]]. */
    if (internals.withRetries) to.levelup(this.level + 1, tx)
    else                     this.leveldown(to.level - 1, tx)
  }

  private def levelup(newLevel: Int, tx: Instant): Unit = if (newLevel > level) {
    level = newLevel; tx.resubmit(this) // resort on lvl change
    outs.foreach(_.levelup(newLevel + 1, tx))
  }

  private def leveldown(newLevel: Int, tx: Instant): Unit = if (newLevel < this.level) {
    level = newLevel; tx.resubmit(this) // resort on lvl change
    getIns.foreach(_.leveldown(newLevel - 1, tx))
  }

  override protected def finalize(): Unit =
    println("fin " + this.id)
}

class Mode()
case object Paused extends Mode
case object Pushing extends Mode
case object Frozen extends Mode

class EmptyValExc(cause: Throwable) extends Throwable(cause)

private object internals {

  @inline def emptyValExc[X](e:Throwable = null): Try[X] = Failure {
    val result = new EmptyValExc(e)
    val index: Int = result.getStackTrace.lastIndexWhere(x =>
      x.getClassName.startsWith("Rendering") || x.getClassName.startsWith("drx.")
    )
    result.setStackTrace(result.getStackTrace.drop(index+1).take(1))
    result
  }

  // there are two kinds of strategies to handle dynamic edges
  val withRetries = false // true = old variant, false = cooler variant

  val activeRx: DynamicVariable[Option[InternalRx[_]]] = new DynamicVariable(None)

//  var uniqueCtr = 0
//  def count: Int = { uniqueCtr += 1; uniqueCtr }
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
