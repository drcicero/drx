package drx

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

/** you can put multiple variable changes into a grouped block,
  * so that at the end of the block all changes will be processed,
  * instead of after each change. */
object atomic {
  def apply[X](changer: => X): X = withInstant { _ => changer }
  var waiting: mutable.Set[String] = mutable.Set()
  private[drx] var postponedEffects = mutable.ListBuffer[() => Unit]()
}

/* private[drx] */object withInstant {
  private[drx] val activeInstant: DynamicVariable[Option[atomic]] = new DynamicVariable(None)

  def apply[X](changer: atomic => X): X = {
    activeInstant.value match {
      case Some(tx) => changer(tx)
      case None     =>
        concreteplatform.startMeasure()
        val tx = new atomic()
        val tmp = activeInstant.withValue(Some(tx)) {
          val tmp = changer(tx)
          tx.processChanges()
          tmp
        }
        concreteplatform.endMeasure()
        tmp
    }
  }
}

/** this context implements glitch-freeness */
private class atomic {
  // drx-interface //////////////////////////////////////////////////

  type InstantRx = graph.DynamicRx[_]

  def markSource(it: InstantRx): Unit = if (!dirtySources.add(it))
    throw new RuntimeException("variable "+it+" can only be set once per transaction.")
  def ensureSource(it: InstantRx): Unit = dirtySources.add(it)

  def markRxShallow(it: InstantRx): Unit =
    if (!dirty.contains(it)) dirty.offer(it)
  def markRx(it: InstantRx): Unit = {
    shouldPush.add(it); if (!dirty.contains(it)) dirty.offer(it) }
  def resubmit(it: InstantRx): Unit = if (dirty.remove(it)) dirty.add(it)

  def runBeforeWait(it: () => Unit): Unit  = beforewait += it
  def runLater(it: () => Unit): Unit  = effects += it

  // private //////////////////////////////////////////////////

  private def cmpLevel(x: InstantRx, y: InstantRx): Int = x.level - y.level

  private[drx] val shouldPush   = mutable.Set[InstantRx]()
  private[drx] val dirtySources = mutable.Set[InstantRx]()
  private[drx] val dirty        = new java.util.PriorityQueue[InstantRx](11, cmpLevel)
//private[drx] val ready        = mutable.Set[InstantRx[_]]() // parallel
  private[drx] val clean        = mutable.Set[InstantRx]()
  private      val effects      = mutable.ListBuffer[() => Unit]() // warning: do not use a set, else similar-but-not-equal effects will be dropped ?
  private      val beforewait   = mutable.Set[() => Unit]()

  @tailrec final private[drx] def processChanges(): Unit = {
    if (DEBUGLOG) println((dirtySources.map(_.toString).mkString(" "),
      toList(dirty).map(_.toString).mkString(" "),
      effects.mkString(" ")))

    // mark dirty Sources >=> propagate >=> debugWriteToDisk
    dirtySources.foreach(it => markRx(it))
    debug.writeToDisk("DIRTY")
    dirtySources.clear()

    processLowerOrEqual(Int.MaxValue)
    debug.writeToDisk("DONE")

    clean.foreach(_.cleanup())
    shouldPush.clear()
    clean.clear()
    debug.writeToDisk("CLEAR")

    beforewait.foreach(_ ())
    beforewait.clear()
    if (atomic.waiting.isEmpty) {
      log += s"eff ${effects.size} ${atomic.postponedEffects.size}; "
      atomic.postponedEffects.foreach(_ ())
      atomic.postponedEffects.clear()
      effects.foreach(_ ())
      effects.clear()
    } else {
      // TODO make a list of postponedEffects with ID and apply one by one
      log += s"ppeff ${effects.size} ${atomic.postponedEffects.size}; ${atomic.waiting}"
      atomic.postponedEffects ++= effects
      effects.clear()
    }

    if (DEBUGLOG) { println(log); log = "" }
    if (dirtySources.nonEmpty || dirty.size() != 0) processChanges()
    else if (DEBUGLOG) println()
  }

  private[drx] def processLowerOrEqual(level: Int): Unit = {
    // process ready
    var head: InstantRx = dirty.peek
    while (head != null && head.level <= level) {
      //    debug.writeToDisk()
      if (internals.withRetries) {
        // with retries the signal might be reevaluated twice, so remove it first
        dirty.remove(head)
        head.reeval()
        if (DEBUGLOG) log += head + "; "
      } else {
        if (DEBUGLOG) log += head + " ("
        head.reeval()
        if (DEBUGLOG) log += "); "
        // without retries the signal should not be reevaluated twice, so remove it later
        dirty.remove(head)
      }
      clean += head
      //    debug.writeToDisk()
      head = dirty.peek
    }
  }

  // debug
  private def toList[T](queue: java.util.PriorityQueue[T]): Set[T] =
    scala.collection.JavaConverters.asScalaIterator(queue.iterator()).toSet
//  private def str(sig: InternalRx[_]): String = sig.level + ":" + sig.id
  private var log = ""
  private val DEBUGLOG = true
}
