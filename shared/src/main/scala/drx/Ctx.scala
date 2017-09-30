package drx

import scala.collection.mutable
import scala.util.DynamicVariable

private object withContext {
  private val activeCtx: DynamicVariable[Option[Ctx]] = new DynamicVariable(None)
  def apply[X](changer: (Ctx) => X): X = {
    activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     =>
        val tx = new Ctx()
        activeCtx.withValue(Some(tx)) {
          val tmp = changer(tx)
          tx.processChanges()
          tmp
        }
    }
  }
}

/** this context implements glitch-freeness */
private class Ctx {
  def markSource(it: EventSource[_]): Unit =
    if (!sources.add(it)) throw new RuntimeException(
      "variable can only be set once per transaction.")
  def markSignal(it: GraphNode[_]): Unit = if (criteria(it)) signalQueue.offer(it)
  def markSink(it: () => Unit): Unit = sideeffects += it

  private val sources = mutable.Set[EventSource[_]]()
  private val signalQueue = new java.util.PriorityQueue[GraphNode[_]](11,
    (x: GraphNode[_], y: GraphNode[_]) => x.level - y.level)
  private val evaluated = mutable.Set[GraphNode[_]]()
  private val sideeffects = mutable.Set[() => Unit]()

  private[drx] def processAllLowerThan(level: Int): Unit = {
    var head: GraphNode[_] = null
    while ({head = signalQueue.peek; head != null && head.level < level}) {
      evaluated += signalQueue.poll
      head.reeval()
      println("did " + str(head) + "; need " + toList(signalQueue).map(str).mkString(" "))
    }
  }

  private[drx] def processChanges(): Unit = {
    println(sources.map(_.id).mkString(" "),
      toList(signalQueue).map(_.id).mkString(" "),
      sideeffects.mkString(" "))
    sources.flatMap(_.getOuts).foreach(markSignal); sources.clear()
    evaluated.clear(); processAllLowerThan(Int.MaxValue); evaluated.clear()
    if (sources.nonEmpty) ??? // sources may not be changed
    sideeffects.foreach(_()); sideeffects.clear()
    if (sources.nonEmpty || signalQueue.size() != 0) processChanges()
    else println()
  }

  private def criteria(it: GraphNode[_]) = !signalQueue.contains(it) &&
    // when having retries, we must allow duplicate evaluations,
    // when not having retries, we must forbid them
    (internals.withRetries || !evaluated.contains(it))

  private def toList[T](queue: java.util.PriorityQueue[T]): Set[T] =
    scala.collection.JavaConverters.asScalaIterator(queue.iterator()).toSet

  private def str(sig: GraphNode[_]): String = sig.level + ":" + sig.id
}