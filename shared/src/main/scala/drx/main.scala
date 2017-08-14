/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.DynamicVariable

object grouped {
  def apply[X](changer: => Unit): Unit = withContext { _ => changer }
}

sealed abstract class Rx[X] private[drx] (name: String) {
  val id: String = "" + internals.count + name
  def now: X = value.get
  def get: X = internals.activeCtx.value.get.getAndSubscribe(this)
  def map[Y](func: X => Y, name: String = ""): Signal[Y] = Signal( func(get), name )
  def mkObserver(callback: X => Unit): Observer[X] = new Observer[X](this, callback, "obs" + id)

//  private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )
  private[drx] var level: Int = 0
  private[drx] var value: Option[X] = None
  private[drx] val out: mutable.Set[Signal[_]] = mutable.Set()
  private[drx] val observers: mutable.Set[Observer[X]] = mutable.Set()
  private[drx] def addObs(obs: Observer[X]): Unit = assertAndThen(!observers.contains(obs))(this.observers += obs)
  private[drx] def removeOut(sig: Signal[_]): Unit = assertAndThen(out.contains(sig))(this.out -= sig)
  private[drx] def removeObs(obs: Observer[X]): Unit = assertAndThen(observers.contains(obs))(this.observers -= obs)
}


sealed class Var[X](init: X, name: String = "") extends Rx[X](name) {
  debug.debugVars(this) = Unit
  value = Some(init)
  def set(newValue: X): Unit = withContext { tx => value = Some(newValue); tx.markDirty(this) }
  def transform(transformer: X => X): Unit = set(transformer(value.get))
  override def toString: String = "Var(" + this.value.toString + ")"
}


object Signal {
  def apply[X](formula: => X, name: String = ""): Signal[X] = new Signal(formula _, name)
}

/** create using [[Signal.apply]] */
sealed class Signal[X] private[drx] (private[drx] val formula: () => X, name: String = "") extends Rx[X](name) {
  debug.debugSigs(this) = Unit

  private[drx] val in: mutable.Set[Rx[_]] = mutable.Set()
  // private[drx] val createdObservers: mutable.Set[Observer[_]] = mutable.Set()
  // override def now: X = { value.getOrElse { this.reeval() } } // TODO
  private[drx] def calcActive: Boolean = observers.nonEmpty || out.nonEmpty

  override private[drx] def addObs(obs: Observer[X]): Unit = {
    val wasActive = calcActive
    super.addObs(obs)
    if (!wasActive) withContext(_.markDirty(this)) // we were just activated!
  }
  override private[drx] def removeOut(sig: Signal[_]): Unit = {
    super.removeOut(sig)
    if (!calcActive) in.foreach(_.removeOut(this))
  }
  override private[drx] def removeObs(obs: Observer[X]): Unit = {
    super.removeObs(obs)
    if (!calcActive) in.foreach(_.removeOut(this))
  }

  private[drx] def reeval(): Unit = {
    if (!calcActive) throw new RuntimeException("should not reeval inactive signal")

//    if (debug.useOwnership) {
//      createdObservers.foreach(obs => obs.kill())
//      createdObservers.clear()
//    }

    val tmpIn = Set() ++ in
    in.clear()
    val result = internals.activeCtx.value.get.reeval(this)
    (tmpIn -- in).foreach(_.removeOut(this))
    this.value = Some(result)
  }
}

/** create using [[Rx.mkObserver]]. */
sealed class Observer[X] private[drx] (private[drx] val observed: Rx[X], private val callback: (X) => Unit, name: String = "") {
  val id: String = "" + internals.count + name
  debug.debugObs(this) = Unit
  // if (debug.useOwnership) Ctx.activeSig.value.get.createdObservers += this
  // if (debug.activateImmediately) observed.addObs(this)

  def isActive: Boolean = observed.observers.contains(this)
  def deactivate(): Unit = if (isActive) observed.removeObs(this)
  def activate(): Unit = if (!isActive) observed.addObs(this)

  private[drx] def trigger(): Unit = callback(observed.value.get)
}

private object internals {
  private var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val activeCtx: DynamicVariable[Option[Ctx]] = new DynamicVariable(None)
}

private object assertAndThen {
  def apply[X](bool: Boolean)(func: => X): X = if (!bool) throw new RuntimeException("invalid state") else func
}
