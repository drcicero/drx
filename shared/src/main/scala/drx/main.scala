/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Success, Try}

object grouped {
  def apply[X](changer: => Unit): Unit = withContext { _ => changer }
}

sealed abstract class Node[X] private[drx](name: String) {
  debug.debugRxs(this) = Unit
  val id: String = "" + internals.count + name

  private[drx] var level: Int = 0
  private def maxOrZero(lst: TraversableOnce[Int]) = try lst.max catch { case _: UnsupportedOperationException => 0 }

  private val outs: mutable.Set[Node[_]] = mutable.Set()
  private[drx] def getOuts: TraversableOnce[Node[_]] = outs
  private[drx] def calcActive: Boolean = outs.nonEmpty
  private[drx] def delOut(sig: Node[_]): Unit = {
    if (!outs.contains(sig)) throw new RuntimeException("redundant delOut from " + this.id + " to "+ sig.id)
    println("  delout " + id + "-= " + sig.id)
    outs -= sig
    if (!calcActive) ins.foreach(_.delOut(this))
  }

  private[drx] val ins: mutable.Set[Node[_]] = mutable.Set()
  private[drx] def linkWith(to: Node[_]): Boolean = {
//    if (this.outs.contains(to)) throw new RuntimeException("redundant addOut from " + this.id + " to "+ to.id)

    to.ins += this
    val newLevel = maxOrZero(to.ins.map(_.level)) + 1
    val levelup = newLevel > to.level
    if (levelup) to.level = newLevel

    val wasActive = this.calcActive
    this.outs += to

    levelup || !wasActive || value.isEmpty
  }
  private[drx] def setupPushTo(to: Node[_]): Unit = {
    println("  " + id + " pushTo " + to.id)
    if (linkWith(to)) { withContext(_.markDirty(this)); throw RetryLater }
  }

  private[drx] var value: Option[X] = None

  // private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )
}

sealed trait Reactive[X] extends Node[X] {
  def now: X = value.get
  def get: X = {
    setupPushTo(internals.activeSig.value.get)
    value.get // OrElse(throw new RuntimeException("Signal " + id + " is empty :(, running " + to.id))
  }
  def map[Y](func: X => Y, name: String = ""): Signal[Y] = Signal( func(get), name )
  def observe(callback: X => Unit): Observer[X] = new Observer(this, callback)
  def fold[Y](init: Y)(comb: (Y, X) => Y): Signal[Y] = {
    // TODO could activate folds without token...
    var tmp: Y = init
    val result = new Signal[Y]({ () => println("fold " + id); tmp = comb(tmp, get); tmp }, "fold" + id) with Interesting[Y]
    result.activate(internals.foldtoken)
    result
  }
}

private[drx] sealed trait Reactor[X] extends Node[X] {
  private[drx] val formula: () => X
  private[drx] def reeval(): Unit = {
    if (!calcActive) return // throw new RuntimeException("should not reeval inactive signal " + id)
    println(s"  $id : ${ins.map(_.id)} -> ${getOuts.map(_.id).toSet}")
    val tmpIn = Set() ++ ins; ins.clear()
    Try(internals.activeSig.withValue(Some(this)) { formula() }) match {
      case Success(v) =>
        (tmpIn -- ins).foreach(_.delOut(this))
        if (v != value) withContext(tx => getOuts.foreach(tx.markDirty))
        value = Some(v)
      case Failure(exc) =>
        ins ++= tmpIn
    }
  }
}

sealed trait Interesting[X] extends Node[X] {
  def deactivate(tok: Token): Unit = { println("  deac " + id); tok.ins -= this; this.delOut(tok) }
  def activate(tok: Token): Unit = if (linkWith(tok)) withContext(_.markDirty(this))
  def activate(): Token = { val token = new Token(""); activate(token); token }
}

sealed class Var[X](init: X, name: String = "") extends Node[X](name) with Reactive[X] {
  value = Some(init)
  def set(newValue: X): Unit = { println(s"set $id ${value.get} to $newValue"); value = Some(newValue); withContext(_.markDirty(this)) }
  def transform(transformer: X => X): Unit = set(transformer(value.get))
  private[drx] override def setupPushTo(to: Node[_]): Unit = linkWith(to)
  override def toString: String = "Var(" + this.value.toString + ")"
}

sealed class Token(name: String) extends Node[Unit](name)

/** create using [[Signal.apply]] */
sealed class Signal[X] private[drx] (private[drx] val formula: () => X, name: String) extends Node[X](name) with Reactor[X] with Reactive[X]
object Signal { def apply[X](formula: => X, name: String = ""): Signal[X] = new Signal(formula _, name) }

/** create using [[Reactive.observe]] */
sealed class Observer[X] private[drx](parent: Reactive[X], callback: X => Unit) extends Node[Unit]("obs") with Reactor[Unit] with Interesting[Unit] {
  private[drx] val formula = () => callback(parent.get)
  level = Int.MaxValue
}

private object internals {
  private var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val activeCtx: DynamicVariable[Option[Ctx]] = new DynamicVariable(None)
  val activeSig: DynamicVariable[Option[Reactor[_]]] = new DynamicVariable(None)
  val foldtoken = new Token("foldtoken")
}