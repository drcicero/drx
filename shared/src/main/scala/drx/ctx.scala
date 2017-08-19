package drx

import scala.collection.mutable
import scala.util.DynamicVariable

private object withContext {
  private val activeCtx: DynamicVariable[Option[Ctx]] = new DynamicVariable(None)
  def apply[X](changer: (Ctx) => Unit): Unit = {
    activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     =>
        val tx = new Ctx()
        activeCtx.withValue(Some(tx)) {
          changer(tx)
          tx.processChanges()
        }
    }
  }
}

private[drx] case object RetryLater extends Throwable

/** this context implements glitch-freeness */
private class Ctx {
  def markVar(it: EventSource[_]): Unit = variables += it
  def markSig(it: DerivedValue[_]): Unit = if (!signalQueue.contains(it)) signalQueue.offer(it)
  def markObs(it: () => Unit): Unit = observers += it

  private val variables: mutable.Set[EventSource[_]] = mutable.Set()
  private val signalQueue = new java.util.PriorityQueue[DerivedValue[_]](11, (x, y) => x.level - y.level)
  private val observers: mutable.Set[() => Unit] = mutable.Set()

  private[drx] def processChanges(): Unit = {
    println(variables, signalQueue, observers)
    variables.flatMap(_.getOuts).foreach(markSig); variables.clear()
    var head: DerivedValue[_] = null
    while ({head = signalQueue.poll; head != null}) {
      println(head.level +" "+ head.id + " of " + qts(signalQueue).map(_.level))
      head.reeval()
    }
    println()
    if (variables.nonEmpty) ??? // variables may not be changed
    observers.foreach(_()); observers.clear()
    if (variables.nonEmpty || signalQueue.size() != 0) processChanges()
  }

  private def qts[T](queue: java.util.PriorityQueue[T]): Set[T] =
      scala.collection.JavaConverters.asScalaIterator(queue.iterator()).toSet
}


//      if (lvl.exists(_.level < head.level)) throw new RuntimeException("did not get smallest element :( " + head.level + " of " + lvl.map(_.level).toList)
