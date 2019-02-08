package drx

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{DynamicVariable, Try}

/** you can put multiple variable changes into a grouped block,
  * so that at the end of the block all changes will be processed,
  * instead of after each change. */
object instantly {
  def apply[X](changer: => X): X = withInstant { _ => changer }
}

private[drx] object withInstant {
  val activeCtx: DynamicVariable[Option[Instant]] = new DynamicVariable(None)

  def apply[X](changer: Instant => X): X = {
    activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     =>
        val tx = new Instant()
        val tmp = activeCtx.withValue(Some(tx)) {
          val tmp = changer(tx)
          tx.processChanges(None)
          tmp
        }
        tmp
    }
  }

//  /** run changer and then return the value of `getValueOf` reactive, but first
//    * ensure its validity by evaluating all other marked reactives of lower level. */
//  def apply[X](getValueOf: InternalRx[X])(changer: (Transaction) => Unit): Try[X] = {
//    activeCtx.value match {
//      case Some(tx) =>
//        changer(tx)
//        tx.tryGetValidValue(getValueOf, None)
//      case None     =>
//        val tx = new Transaction()
//        activeCtx.withValue(Some(tx)) {
//          changer(tx)
//          tx.processChanges(Some(getValueOf)).get
//        }
//    }
//  }

}

/** this context implements glitch-freeness */
private class Instant {
  def markSource(it: InternalRx[_]): Unit = {
    if (!dirtySources.add(it))
      throw new RuntimeException("variable "+it.id+" can only be set once per transaction.")
  }
  def ensureSource(it: InternalRx[_]): Unit = !dirtySources.add(it)
  def markRx(it: InternalRx[_]): Unit =
//    if (it.ins.forall(clean.contains)) ready += it else
    { it.shouldPush = true; if (!dirty.contains(it)) dirty.offer(it) }
  def markRxShallow(it: InternalRx[_]): Unit =
    if (!dirty.contains(it)) dirty.offer(it)
  def runLater(it: () => Unit): Unit  = effects += it
  def resubmit(it: InternalRx[_]): Unit = if (dirty.remove(it)) dirty.add(it)

  private val dirtySources = mutable.Set[InternalRx[_]]()
  private def cmpLevel(x: InternalRx[_], y: InternalRx[_]) = x.level - y.level
  private[drx] val dirty = new java.util.PriorityQueue[InternalRx[_]](11, cmpLevel)
//  private[drx] val ready = mutable.Set[InternalRx[_]]()
  private[drx] val clean = mutable.Set[InternalRx[_]]()
  private[drx] val pastDirtySources = mutable.Set[InternalRx[_]]() // debug
  private val effects = mutable.ListBuffer[() => Unit]() // warning: do not use a set, else similar-but-not-equal effects will be dropped ?

  @tailrec final private[drx] def processChanges[X](getValueOf: Option[InternalRx[X]]): Option[Try[X]] = {
    if (LOG) println((dirtySources.map(_.id).mkString(" "),
      toList(dirty).map(_.id).mkString(" "),
      effects.mkString(" ")))

    // mark dirty Sources >=> propagate >=> debugWriteToDisk
    dirtySources.foreach(it => markRx(it))
    pastDirtySources ++= toList(dirty)
    dirtySources.clear()

    debug.writeToDisk("DIRTY")
    processLowerOrEqual(Int.MaxValue)

    debug.writeToDisk("DONE")
    pastDirtySources.clear()

    effects.foreach(_ ())
    val result = getValueOf.map(_.value)
    val cleared = internals.emptyValExc()
    clean.foreach { x =>
      x.shouldPush = false
      if (!x.remember) x.value = cleared
    }; clean.clear()
    debug.writeToDisk("CLEAR")

    effects.clear()
    if (LOG) { println(log); log = "" }
    if (dirtySources.nonEmpty || dirty.size() != 0)
      processChanges(getValueOf)
    else {
      if (LOG) println()
      result
    }
  }

  private[drx] def processLowerOrEqual(level: Int): Unit = {
    // process ready
    var head: InternalRx[_] = dirty.peek
    while (head != null && head.level <= level) {
      head.debugEvaluating = true
      //    debug.writeToDisk()
      if (internals.withRetries) {
        // with retries the signal might be reevaluated twice, so remove it first
        dirty.remove(head)
        head.reeval()
        if (LOG) log += head.id + "; "
      } else {
        if (LOG) log += head.id + " ("
        head.reeval()
        if (LOG) log += "); "
        // without retries the signal should not be reevaluated twice, so remove it later
        dirty.remove(head)
      }
      //    debug.writeToDisk()
      head.debugEvaluating = false
      head = dirty.peek
    }

//    // collect ready
//    while ({
//      head = dirty.poll
//      head != null && head.level <= level
//    }) ready.offer(head)

//    println("ready " +ready.map(str).mkString(" ")+ "; " +
//      "need " + toList(dirty).map(str).mkString(" "))
  }

  // debug
  private def toList[T](queue: java.util.PriorityQueue[T]): Set[T] =
    scala.collection.JavaConverters.asScalaIterator(queue.iterator()).toSet
  private def str(sig: InternalRx[_]): String = sig.level + ":" + sig.id
  private var log = ""
  private val LOG = false
}
