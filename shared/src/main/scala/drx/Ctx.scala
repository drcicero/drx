package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Try}

/** you can put multiple variable changes into a grouped block,
  * so that at the end of the block all changes will be processed,
  * instead of after each change. */
object transact {
  def apply[X](changer: => X): X = withTransaction { _ => changer }
}

private[drx] object withTransaction {
  val activeCtx: DynamicVariable[Option[Transaction]] = new DynamicVariable(None)

  def apply[X](changer: (Transaction) => X): X = {
    activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     =>
        val tx = new Transaction()
        activeCtx.withValue(Some(tx)) {
          val tmp = changer(tx)
          tx.processChanges(None)
          tmp
        }
    }
  }

  /** run changer and then return the value of `getValueOf` reactive, but first
    * ensure its validity by evaluating all other marked reactives of lower level. */
  def apply[X](getValueOf: InternalRx[X])(changer: (Transaction) => Unit): Try[X] = {
    activeCtx.value match {
      case Some(tx) =>
        changer(tx)
        tx.tryGetValidValue(getValueOf, None)
      case None     =>
        val tx = new Transaction()
        activeCtx.withValue(Some(tx)) {
          changer(tx)
          tx.processChanges(Some(getValueOf)).get
        }
    }
  }

}

/** this context implements glitch-freeness */
private class Transaction {
  def markSource(it: InternalRx[_]): Unit = {
    if (!dirtySources.add(it))
      throw new RuntimeException("variable "+it.id+" can only be set once per transaction.")
  }
  def markRx(it: InternalRx[_]): Unit =
//    if (it.ins.forall(clean.contains)) ready += it else
    if (!dirty.contains(it)) dirty.offer(it)
  def runLater(it: () => Unit): Unit  = effects += it
  def resubmit(it: InternalRx[_]): Unit = if (dirty.remove(it)) dirty.add(it)

  private val dirtySources = mutable.Set[InternalRx[_]]()
  private def cmpLevel(x: InternalRx[_], y: InternalRx[_]) = x.level - y.level
  private[drx] val dirty = new java.util.PriorityQueue[InternalRx[_]](11, cmpLevel)
//  private[drx] val ready = mutable.Set[InternalRx[_]]()
  private[drx] val clean = mutable.Set[InternalRx[_]]()
  private val effects = mutable.Set[() => Unit]()

  private[drx] def processChanges[X](getValueOf: Option[InternalRx[X]]): Option[Try[X]] = {
    if (LOG) println((dirtySources.map(_.id).mkString(" "),
      toList(dirty).map(_.id).mkString(" "),
      effects.mkString(" ")))
    dirtySources.foreach(it => markRx(it))
    dirtySources.clear()

    processAllLowerTo(Int.MaxValue)
    debug.writeToDisk()

    clean.clear()
    effects.foreach(_ ())
    effects.clear()
    if (LOG) { println(log); log = "" }
    if (dirtySources.nonEmpty || dirty.size() != 0) processChanges(getValueOf)
    else {
      if (LOG) println()
      getValueOf.map(_.value)
    }
  }

  private[drx] def tryGetValidValue[X](rx: InternalRx[X], outer: Option[InternalRx[_]]): Try[X] = {
    /** there are two kinds of strategies to handle this situation,
      * you can choose between over the global variable [[internals.withRetries]]. */
    if (internals.withRetries) {
      // to avoid glitches, the value is only valid, if there are no
      // marked reactives lower or equal to our level.
      if (dirty.size != 0 && dirty.peek.level <= rx.level) {
        // We stop evaluation by throwing an exception, if we are currently
        // evaluation something, we have to remark it to be evaluated later.
        outer.foreach(markRx)
        internals.TheEmptyStream.get
      }
    } else {
      // We process the lower signals now.
      processAllLowerTo(rx.level)
//      if (rxQueue.contains(rx)) {
//        rxQueue.remove(rx)
//        rx.reeval()
//      }
    }

    // we have ensured that the value is valid, so we can return it
    rx.value
  }

  private def processAllLowerTo(level: Int): Unit = {
    // process ready
    var head: InternalRx[_] = null
    while ({
      head = dirty.peek
      head != null && head.level <= level
    }) {
      if (internals.withRetries) {
        dirty.remove(head) // with retries the signal might be reevaluated twice, so remove it first
        head.reeval()
        if (LOG) log += head.id + "; "
      } else {
        if (LOG) log += head.id + " ("
        head.reeval()
        if (LOG) log += "); "
        dirty.remove(head) // without retries the signal should not be reevaluated twice, so remove it later
      }
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
  private val LOG = true
}
