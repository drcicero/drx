/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Success, Try}

/** A node in the directed graph of reactives. */
private[drx] class Reactive[X] private[drx](private[drx] val isEvent: Boolean, name: String) {
  debug.debugRxs(this) = Unit

  val id: String = "" + internals.count + name
//  private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )

  // INVARIANT: outs.forall(out => out.level > this.level)
  // INVARIANT: ins. forall(in  => in.level  < this.level)
  // INVARIANT: !this.isNeeded <==> debugRxs.forall( x => !x.out.contains(this) )
  // INVARIANT: this.frozen    ==> ins.isEmpty
  // INVARIANT: !ins.forall(_.frozen)

  protected var value: Option[X] = None
  protected var level: Int = 0
  protected var father: Option[Reactive[_]] = None // permanent incoming
  protected var isDead: Boolean = false
  protected var forceNeeded: Boolean = false
  protected val ins: mutable.Set[Reactive[_]] = mutable.Set() // volatile incoming
  protected val outs: mutable.Set[Reactive[_]] = mutable.Set() // volatile outgoing
  private[drx] var formula: ()=>X = () => value.getOrElse(throw ReadEmptyStream)

  private[drx] def getLevel: Int = level
  private[drx] def isNeeded: Boolean = outs.nonEmpty || forceNeeded
  private[drx] def getOuts: Set[Reactive[_]] = outs.toSet
  private[drx] def getIns: Set[Reactive[_]] = ins.toSet ++ father
  private[drx] def kill(): Unit = {
    isDead = true
    outs.foreach { it => if ((it.ins ++ it.father).forall(_.isDead)) it.kill() }
    formula = () => value.getOrElse(throw ReadEmptyStream)
    father = None
    ins.clear()
  }

  private[drx] def pushto(to: Reactive[_]): Unit = {
    if (!this.outs.contains(to)) println(s"  pushto from $id to ${to.id}")

    val wasInactive = !this.isNeeded
    val dolevelup   =  internals.withRetries && this.level + 1 > to.level
    val doleveldown = !internals.withRetries && to.level - 1 < this.level

    if (dolevelup)     to.levelup(this.level + 1)
    if (doleveldown) this.leveldown(to.level - 1)

    this.outs += to
    if (!to.father.contains(this)) to.ins += this

    if (wasInactive) this.activatedby(to)
    else if (dolevelup)   withContext { tx => tx.markSignal(to); throw RetryLater }
    else if (doleveldown) withContext { tx => tx.processAllLowerThan(to.level) }
  }

  private def activatedby(to: Reactive[_]): Unit = withContext { tx =>
    tx.markSignal(this)
    if (internals.withRetries)
      throw RetryLater
    else
    /** actually, when without retrying, the above marked sig must not
      * [[Ctx.markSignal]] its outs, or we would trigger [[to]] twice.
      * Those duplicate evaluations are catched via [[Ctx.evaluated]]. */
      tx.processAllLowerThan(to.level)
  }
  private def leveldown(newLevel: Int): Unit = {
    level = newLevel
    (ins ++ father).foreach(_.leveldown(newLevel - 1))
  }
  private def levelup(newLevel: Int): Unit = {
    level = newLevel
    outs.foreach(_.levelup(newLevel + 1))
  }

  private[drx] def unpushto(to: Reactive[_]): Unit = {
//    if (!outs.contains(to)) throw new RuntimeException(s"redundant broke from $id to ${to.id}") else
    println(s"  broke from $id to ${to.id}")
    outs -= to
    checkStillActive()
  }
  private def checkStillActive(): Unit =
    if (!isNeeded) (ins ++ father).foreach(_.unpushto(this))

//  private[drx] def reeval(): Unit = withContext(tx => outs.foreach(tx.markSignal))
  private[drx] def reeval(): Unit = {
    if (!isNeeded) return // throw new RuntimeException("should not reeval inactive signal " + id)
    val tmpIn = Set() ++ ins; ins.clear()
    Try(internals.activeSig.withValue(Some(this)) { formula() }) match {
      case Success(v) =>
        (tmpIn -- ins).foreach(_.unpushto(this))
        if (isEvent || v != value) withContext { tx =>
          value = Some(v)
          outs.foreach(tx.markSignal)
          if (isEvent) tx.markSink(() => value = None)
        }
      case Failure(exc) =>
        (tmpIn -- ins).foreach(_.unpushto(this)) // TODO is this important?
        if (exc != ReadEmptyStream && exc != RetryLater) throw exc
    }
  }

  override protected def finalize(): Unit = println("fin " + this.id)
}

///** Streams are a series of singular events.
//  * They only may have values during transactions, in which they fire..
//  * Sampling outside of transactions, always fails. */

private[drx] case object RetryLater extends Throwable
case object ReadEmptyStream extends Throwable

private object internals {
  private var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val withRetries = false // true = old variant, false = cooler variant
  val activeSig: DynamicVariable[Option[Reactive[_]]] = new DynamicVariable(None)
  val dummyOwner = new VarOwner {}

  def checksafety(me: Reactive[_], owner: VarLike): Unit = {
    val ancestry: mutable.Set[Reactive[_]] = mutable.Set()
    def transitiveIns(it: Reactive[_]): Unit = {
      if (ancestry.add(it)) it.getIns.foreach(transitiveIns)
    }
    transitiveIns(me)
    val vars = ancestry.collect { case it: EventSource[_] => it }
    if (vars.size > 1 && !vars.subsetOf(owner.getEventsources))
      throw new RuntimeException("You are creating a fold over multiple variables." +
        " The fold will live until all incoming variables are dead.." +
        " You must specify the VarOwner of all Variables that influence this fold." +
        " for example like: a = owner.mkVar(); b = owner.mkVar() _.fold(0, owner)(_+_) ." +
        " The fold will be then be collected when this VarOwner is collected.")
  }
}
