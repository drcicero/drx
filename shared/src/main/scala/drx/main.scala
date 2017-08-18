/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Success, Try}

object grouped {
  def apply[X](changer: => Unit): Unit = withContext { _ => changer }
}

sealed abstract class GraphNode[X] private[drx](name: String) {
  debug.debugRxs(this) = Unit

  val id: String = "" + internals.count + name

  private[drx] var level: Int = 0
  private[drx] val outs: mutable.Set[DerivedValue[_]] = mutable.Set()
  private[drx] val ins: mutable.Set[GraphNode[_]] = mutable.Set()
  private[drx] def calcActive: Boolean = outs.nonEmpty
  private[drx] def getOuts: Set[DerivedValue[_]] = outs.toSet
  private[drx] def unsubscribe(sig: DerivedValue[_]): Unit = {
    if (!outs.contains(sig)) throw new RuntimeException("redundant unsubscribe from " + this.id + " to "+ sig.id)
    else println("  unsubscribe from " +this.id+" to "+sig.id)
    outs -= sig
  }

  private def maxOrZero(lst: TraversableOnce[Int]) = try lst.max catch { case _: UnsupportedOperationException => 0 }
  private[drx] def linkWith(to: DerivedValue[_])(ifneedsreevaluation: => Unit): Unit = {
    to.ins += this
    val newLevel = maxOrZero(to.ins.map(_.level)) + 1
    val levelup = newLevel > to.level
    if (levelup) to.level = newLevel

    val wasActive = this.calcActive
    if (!this.outs.contains(to)) println("  subscribe from "+this.id +" to "+to.id)
    this.outs += to

    if (levelup || !wasActive || value.isEmpty) ifneedsreevaluation
  }
  private[drx] def subscribe(to: DerivedValue[_]): Unit
  private[drx] var value: Option[X] = None

  // private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )
}

sealed trait Stream[X] extends GraphNode[X] {
  def now: X = value.get
  def get: X = { subscribe(internals.activeSig.value.get); value.get }
  def map[Y](func: X => Y, name: String = ""): Signal[Y] = Signal( func(get), name )
  def fold[Y](init: Y)(comb: (Y, X) => Y): Signal[Y] = {
    val that = this
    var tmp: Y = init
    val result = new Signal[Y]({ () => tmp = comb(tmp, get); tmp }, "fold") with Startable[Y] { override def parent: Stream[X] = that }
    result.start()
    result
  }
  /** you can register a callback via [[foreach]], that starts immediately. */
  def mkObs(callback: X => Unit): Callback[X] = new Callback(this, callback)
  /** you can create a callback via [[mkObs]], that is deactive at the beginning and can be started later. */
  def foreach(callback: X => Unit): Callback[X] = {
    val tmp = new Callback(this, callback)
    tmp.start()
    tmp
  }
}

sealed trait DerivedValue[X] extends GraphNode[X] {
  override private[drx] def unsubscribe(sig: DerivedValue[_]) = { super.unsubscribe(sig); checkStillActive() }
  override private[drx] def subscribe(to: DerivedValue[_]): Unit =
    linkWith(to) { withContext(_.markSig(this)); throw RetryLater }
  private[drx] def checkStillActive() = if (!calcActive) ins.foreach(_.unsubscribe(this))
  private[drx] val formula: () => X
  private[drx] def reeval(): Unit = {
    if (!calcActive) return // throw new RuntimeException("should not reeval inactive signal " + id)
    val tmpIn = Set() ++ ins; ins.clear()
    Try(internals.activeSig.withValue(Some(this)) { formula() }) match {
      case Success(v) =>
        (tmpIn -- ins).foreach(_.unsubscribe(this))
        if (v != value) withContext(tx => getOuts.foreach(tx.markSig))
        value = Some(v)
      case Failure(exc) =>
        // ins ++= tmpIn
        if (exc != RetryLater) throw exc
    }
  }
}

sealed class Variable[X](init: X, name: String = "") extends GraphNode[X](name) with Stream[X] {
  value = Some(init)
  def set(newValue: X): Unit = { println(s"set $id ${value.get} to $newValue"); value = Some(newValue); withContext(_.markVar(this)) }
  def transform(transformer: X => X): Unit = set(transformer(value.get))
  override private[drx] def subscribe(to: DerivedValue[_]): Unit = linkWith(to) {}
  override def toString: String = "Var(" + this.value.toString + ")"

  private var frozen = true
  def isFrozen: Boolean = frozen
  def freeze(): Unit = frozen = true
}

/** create using [[Signal.apply]] */
sealed class Signal[X] private[drx] (private[drx] val formula: () => X, name: String) extends GraphNode[X](name) with DerivedValue[X] with Stream[X]
object Signal { def apply[X](formula: => X, name: String = ""): Signal[X] = new Signal(formula _, name) }

/** create using [[Stream.mkObs]] */
sealed class Callback[X] private[drx](private[drx] val parent: Stream[X], callback: X => Unit) extends GraphNode[Unit]("obs") with DerivedValue[Unit] with Startable[Unit] {
  private[drx] val formula = () => { val tmp = parent.get; withContext(_.markObs{() => callback(tmp)}) }
  level = Int.MaxValue // run as late as possible
}

sealed trait Startable[X] { self: DerivedValue[X] =>
  var active = false
  override private[drx] def calcActive = active
  def start(): Unit = if (!active) {
    active = true
    withContext { tx =>
      try parent.subscribe(this)
      catch { case RetryLater =>
//        println("retrylater " + this.id + " " + parent.id)
//        tx.markSig(this)
      }
    }
  }
  def stop(): Unit = if (active) { active = false; parent.unsubscribe(this) }
  private[drx] def parent: Stream[_]
}

private object internals {
  private var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val activeSig: DynamicVariable[Option[DerivedValue[_]]] = new DynamicVariable(None)
}