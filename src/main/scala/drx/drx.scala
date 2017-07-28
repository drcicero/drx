import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.util.DynamicVariable

/** Created by david on 12.05.17. */

package object drx {
  private var uniqueCtr = 0
  private val theActiveSignal: DynamicVariable[Option[Signal[_]]] = new DynamicVariable(None)
// private val theActiveTurn:   DynamicVariable[Option[Turn]]      = new DynamicVariable(None)


  abstract class Rx[X](name: String = "") {
    val id: String = "" + (if (name != "") name else {uniqueCtr += 1; uniqueCtr})
    def now: X = value.get
    def get: X = {
      val parent = theActiveSignal.value.getOrElse { throw new RuntimeException("invalid state") }
      parent.in += this; this.out += parent
      value.get
    }
    def map[Y](func: X => Y, name: String = ""): Signal[Y] = new Signal( () => func(get), name )
    def onChange(callback: X => Unit, onDisconnect: () => Unit = () => {}): Observer[X] = {
      new Observer(this, callback, onDisconnect)
    }

//    private[drx] var level = 0
    private[drx] var value: Option[X] = None
    private[drx] val out: mutable.Set[Signal[_]] = mutable.Set()
    private[drx] val observers: mutable.Set[Observer[X]] = mutable.Set()
    private[drx] def isActive: Boolean = observers.nonEmpty || out.exists(it => it.isActive)
    private[drx] def addOut(sig: Signal[_]): Unit = this.out += sig
    private[drx] def addObs(obs: Observer[X]): Unit = this.observers += obs
    private[drx] def removeOut(sig: Signal[_]): Unit = this.out -= sig
    private[drx] def removeObs(obs: Observer[X]): Unit = this.observers -= obs
  }


  class Var[X](init: X, name: String = "") extends Rx[X](name) {
    debugVars(this) = Unit
    this.value = Some(init)
    def set(newValue: X): Unit = {
//      val turn = theActiveTurn.value.getOrElse { throw new RuntimeException("invalid state") }
      println(s"$name := ${newValue.toString} ($out, $observers)")
      value = Some(newValue)
      out.foreach(it => it.reeval())
      observers.foreach(obs => obs.callback(newValue))
    }
    def transform(transformer: X => X): Unit = set(transformer(value.get))
    override def toString: String = "Var(" + this.value.toString + ")"
  }


  class Signal[X](private val formula: () => X, name: String = "") extends Rx[X](name) {
    debugSigs(this) = Unit

    // override def now: X = { value.getOrElse { this.reeval() } }
    override def get: X = {
      val parent = theActiveSignal.value.getOrElse { throw new RuntimeException("invalid state") }
      parent.in += this; this.out += parent
      value.getOrElse { reeval() }
    }

    private[drx] val in: mutable.Set[Rx[_]] = mutable.Set()
    private[drx] val createdObservers: mutable.MutableList[Observer[_]] = mutable.MutableList()

    override private[drx] def addOut(sig: Signal[_]): Unit = { super.addOut(sig); checkNowActive() }
    override private[drx] def addObs(obs: Observer[X]): Unit = { super.addObs(obs); checkNowActive() }
    override private[drx] def removeOut(sig: Signal[_]): Unit = { super.removeOut(sig); checkStillActive() }
    override private[drx] def removeObs(obs: Observer[X]): Unit = { super.removeObs(obs); checkStillActive() }

    private def checkNowActive(): Unit = if (isActive) { reeval() }
    private def checkStillActive(): Unit = if (!isActive) {
      value = None
      in.foreach { dep => dep.removeOut(this) }
    }

    private[drx] def reeval(): X = {
      val tmp = innerReeval()
      this.value = Some(tmp)
      out.foreach(sig => sig.reeval())
      observers.foreach(obs => obs.callback(tmp))
      this.value = None
      tmp
    }

    private def innerReeval(): X = {
      if (!isActive) throw new RuntimeException("cannot reeval inactive signal")

      val tmpIn = Set() ++ in
      in.clear()

      val result = theActiveSignal.withValue(Some(this)) {
        createdObservers.foreach(obs => obs.disconnect())
        createdObservers.clear()
        formula()
      }

      (tmpIn -- in).foreach { dep =>
        if (!dep.out.contains(this)) throw new RuntimeException("invalid state")
        dep.removeOut(this)
      }

      result
    }
  }


  class Observer[X](val observed: Rx[X], val callback: (X) => Unit, val onDisconnect: () => Unit) {
    val id: String = "" + {uniqueCtr += 1; uniqueCtr}
    debugObs += this.id -> this
    theActiveSignal.value map { parent => parent.createdObservers += this }
    observed.addObs(this)
    def disconnect(): Unit = {
      debugObs.remove(this.id) getOrElse { throw new RuntimeException("could not delete observer, not found " + this.id) }
      observed.removeObs(this)
      onDisconnect()
    }
  }

//  private class Turn() {
//    private var phase1 = mutable.PriorityQueue[Signal[_]]()
//    private var phase2 = ListBuffer[() => Unit]()
//    def dirty(signal: Traversable[Signal[_]]): Unit = phase1 ++= signal
//    def isdirty(signal: Signal[_]): Unit = phase1.sameElements(Seq(signal))
//    def schedule(f: () => Unit): Unit = phase2 += f
//    def commit(): Unit = phase2.foreach(_.apply)
//    def tick(): Unit = {
//      val top = phase1.dequeue
//      try top.reeval() catch { case DependeciesChanged => top.level = }
//    }
//  }
//  private def getOrCreateTurn[X](f: () => Unit): Unit = {
//    theActiveTurn.value match {
//      case Some(x) => x.schedule(f)
//      case None    => transaction(f)
//    }
//  }
//  case object DependeciesChanged extends Throwable

  //////////////////////////////////////////////////////////////////////////////
  // ghost testing / debugging variables
  private[drx] class NoMap[K, V] {
    def update(a: K, b: V): Unit = Unit
    def map[R](x: (K, V) => R): List[R] = List.empty
    def size = 0
  }
//   type PotentialWeakHashMap[K,V] = scala.collection.mutable.WeakHashMap[K,V]
  private[drx] type PotentialWeakHashMap[K,V] = NoMap[K,V]
  private[drx] val debugSigs = new PotentialWeakHashMap[Signal[_], Unit]
  private[drx] val debugVars = new PotentialWeakHashMap[Var[_], Unit]
  val debugObs  = mutable.Map[String, Observer[_]]() // can be made ghost soon, too!
  //////////////////////////////////////////////////////////////////////////////
}
