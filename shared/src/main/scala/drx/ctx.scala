package drx

import scala.collection.mutable
import scala.util.DynamicVariable

private object withContext {
  def apply[X](changer: (Ctx) => Unit): Unit = {
    internals.activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     => val tx = new Ctx()
        internals.activeCtx.withValue(Some(tx)) {
          changer(tx)
          tx.loop()
        }
        tx.end()
    }
  }
}

private object signalComparator extends java.util.Comparator[Signal[_]] {
  override def compare(x: Signal[_], y: Signal[_]): Int = x.level - y.level
}

private case class EvalThisFirst(rx: Signal[_]) extends Throwable

/** this context implements glitch-freeness */
private class Ctx {
  def reeval[X](sig: Signal[X]): X = Ctx.activeSig.withValue(Some(sig)) { sig.formula() }

  def markDirty(rx: Rx[_]): Unit = {
    observers ++= rx.observers
    enqueue(rx.out)
    rx match {
      case (a: Signal[_]) => enqueue(Seq(a))
      case _ =>
    }
  }

  def getAndSubscribe[X](rx: Rx[X]): X = rx match {
    case vari: Var[X] =>
      val child = Ctx.activeSig.value.get
      child.in += vari
      vari.out += child
      vari.value.get

    case sig: Signal[X] =>
      val child = Ctx.activeSig.value.get
      if (!child.in.contains(sig)) {
        val wasActive = sig.calcActive
        child.in += sig
        sig.out += child
        val level = child.in.map( x => x.level ).max + 1
        if (!wasActive || level > child.level) {
          child.level = level
          throw EvalThisFirst(sig)
        }
      }
      sig.value.getOrElse(throw new RuntimeException("Signal " + sig.id + " is empty :(, running " + child.id))
  }

  private val levelQueue = new java.util.PriorityQueue[Signal[_]](11, signalComparator)
  private val observers: mutable.Set[Observer[_]] = mutable.Set()
  private def enqueue(xs: TraversableOnce[Signal[_]]): Unit = xs.filter(!levelQueue.contains(_)).foreach(levelQueue.offer)

  private[drx] def loop(): Unit = {
    var head: Signal[_] = null
    while ({head = levelQueue.poll; head != null}) if (head.calcActive) {
//      val lvl = scala.collection.JavaConverters.asScalaIterator(levelQueue.iterator())
//      println(head.level +" "+ head.id + " of " + lvl.map(_.level).toList)
//      if (lvl.exists(_.level < head.level)) throw new RuntimeException(
//        "did not get smallest element :( " + head.level + " of " + lvl.map(_.level).toList)
      try {
        head.reeval() // this may fail
        observers ++= head.observers
        enqueue(head.out)
      } catch { case EvalThisFirst(rx) => enqueue(Set(rx)) }
    }
  }

  private[drx] def end(): Unit = {
    val obs = observers.filter(_.isActive)
    if (obs.nonEmpty) grouped { obs.foreach(_.trigger()) }
  }
}

private object Ctx {
  val activeSig: DynamicVariable[Option[Signal[_]]] = new DynamicVariable(None)
}
