import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.util.DynamicVariable

/** Created by david on 12.05.17. */

package object drx {
  private var uniqueCtr = 0
  private val activeSig:DynamicVariable[Option[Signal[_]]] = new DynamicVariable(None)
//  private val activeTx: DynamicVariable[Option[Tx]]      = new DynamicVariable(None)


  abstract class Rx[X](name: String = "") {
    private val dummy = Array.tabulate(1000 * 100)( i => i )

    val id: String = "" + name + {uniqueCtr += 1; uniqueCtr}
    def now: X = value.get
    def get: X = {
      val parent = activeSig.value.getOrElse { throw new RuntimeException("invalid state") }
      parent.in += this; this.out += parent
      value.get
    }
    def map[Y](func: X => Y, name: String = ""): Signal[Y] = signal( func(get), name )
    def onChange(callback: X => Unit): Observer[X] = {
      new Observer(this, callback, id + ".obs")
    }

    private[drx] var value: Option[X] = None
    private[drx] val out: mutable.Set[Signal[_]] = mutable.Set()
    val observers: mutable.Set[Observer[X]] = mutable.Set()
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
//      println(s"$name := ${newValue.toString} ($out, $observers)")
      value = Some(newValue)
      out.foreach(it => it.reeval())
      observers.foreach(obs => obs.callback(newValue))
    }
    def transform(transformer: X => X): Unit = set(transformer(value.get))
    override def toString: String = "Var(" + this.value.toString + ")"
  }


  def signal[X](formula: => X, name: String = ""): Signal[X] = new Signal(formula _, name)
  class Signal[X](private val formula: () => X, name: String = "") extends Rx[X](name) {
    debugSigs(this) = Unit

    // override def now: X = { value.getOrElse { this.reeval() } }
    override def get: X = {
      val parent = activeSig.value.getOrElse { throw new RuntimeException("invalid state") }
      parent.in += this; this.out += parent
      value.getOrElse { reeval() }
    }

//    private[drx] val level: Int = 0
    private[drx] val in: mutable.Set[Rx[_]] = mutable.Set()
    private[drx] val createdObservers: mutable.MutableList[Observer[_]] = mutable.MutableList()

    override private[drx] def addOut(sig: Signal[_]): Unit = { super.addOut(sig); checkNowActive() }
    override private[drx] def addObs(obs: Observer[X]): Unit = { super.addObs(obs); checkNowActive() }
    override private[drx] def removeOut(sig: Signal[_]): Unit = { super.removeOut(sig); checkStillActive() }
    override private[drx] def removeObs(obs: Observer[X]): Unit = { super.removeObs(obs); checkStillActive() }

    private def checkNowActive(): Unit = if (isActive) { reeval() }
    private def checkStillActive(): Unit = if (!isActive) {
      value = None
      in.foreach { parent => parent.removeOut(this) }
    }

    private[drx] def reeval(): X = {
      val tmp = innerReeval()
      this.value = Some(tmp)
      out.foreach(sig => sig.reeval())
      observers.foreach(obs => obs.callback(tmp))
      tmp
    }

    private[drx] def innerReeval(): X = {
      if (!isActive) throw new RuntimeException("cannot reeval inactive signal")

      val tmpIn = Set() ++ in
      in.clear()

      val result = activeSig.withValue(Some(this)) {
//        createdObservers.foreach(obs => obs.disconnect())
//        createdObservers.clear()
        formula()
      }

      (tmpIn -- in).foreach { dep =>
        if (!dep.out.contains(this)) throw new RuntimeException("invalid state")
        dep.removeOut(this)
      }

      result
    }
  }


  class Observer[X](val observed: Rx[X], val callback: (X) => Unit, name: String = "") {
    val id: String = "" + name + {uniqueCtr += 1; uniqueCtr}
    var isActive = true
    debugObs(this) = Unit
//    activeSig.value map { parent => parent.createdObservers += this }
    observed.addObs(this) // TODO activate immediatly?

    def deactivate(): Unit = {
      if (!isActive) throw new RuntimeException("cannot deactivate deactived observer")
      observed.removeObs(this)
      isActive = false
    }

    def activate(): Unit = {
      if (isActive) throw new RuntimeException("cannot activate actived observer")
      observed.addObs(this)
      isActive = true
    }
  }

//  private case class Change[X](vari: Var[X], next: X)
//  private class Tx(changer: () => Unit) { // Transaction
//    def set(ch: Change[_]): Unit = changes += ch
//
//    private val changes: mutable.Set[Change[_]] = mutable.Set[Change[_]]()
//    activeTx.withValue(Some(this)) { changer() }
//    changes.foreach { case Change(sig, value) => sig.value = Some(value) }
//
//    implicit object ordering extends Ordering[Signal[_]] {
//      override def compare(x: Signal[_], y: Signal[_]): Int = x.level < y.level
//    }
//    private var levelQueue = mutable.PriorityQueue[Signal[_]]()
//    private var observers  = mutable.Set[Observer[_]]()
//    changes.foreach(observers ++= _.vari.observers)
//    changes.foreach(levelQueue ++= _.vari.out)
//    while (levelQueue.nonEmpty) {
//      val head = levelQueue.dequeue
//      val tmp = head.reeval()
//    }
//
//    def isdirty(signal: Signal[_]): Unit = phase1.sameElements(Seq(signal))
//    def schedule(f: () => Unit): Unit = phase2 += f
//    def commit(): Unit = phase2.foreach(_.apply)
//    def tick(): Unit = {
//      val top = phase1.dequeue
//      try top.reeval() catch { case DependeciesChanged => top.level = }
//    }
//  }
//  private def transact[X](f: () => Unit): Unit = {
//    activeTx.value match {
//      case Some(x) => x.schedule(f)
//      case None    => new Tx(f)
//    }
//  }
//  case object DependeciesChanged extends Throwable

  def printless(): Unit = {
    println(compat2.heapSize())
//    println("v=" + debugVars.size + " s=" + debugSigs.size +
//      " o=" + debugObs.size + " heap=" + compat2.heapSize() / 10000000)
  }

  def doit() = {
    printless()
    for (i <- 0 to 5) compat2.gc()
    printless()
  }

  //////////////////////////////////////////////////////////////////////////////
  private[drx] val debugSigs = compat2.PotentialWeakHashMap[Signal[_], Unit]()
  private[drx] val debugVars = compat2.PotentialWeakHashMap[Var[_], Unit]()
  private[drx] val debugObs  = compat2.PotentialWeakHashMap[Observer[_], Unit]() // can be made PotentialWeakHashMap soon, too!
  //////////////////////////////////////////////////////////////////////////////
}
