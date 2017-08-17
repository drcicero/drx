package drx

import scala.collection.mutable
import scala.util.DynamicVariable

private object withContext {
  def apply[X](changer: (Ctx) => Unit): Unit = {
    internals.activeCtx.value match {
      case Some(tx) => changer(tx)
      case None     =>
        val tx = new Ctx()
        internals.activeCtx.withValue(Some(tx)) {
          changer(tx)
          tx.loop()
        }
    }
  }
}

private[drx] case object RetryLater extends Throwable

/** this context implements glitch-freeness */
private class Ctx {
  def markDirty(rx: Node[_]): Unit = rx match {
    case (it: Token)      =>
    case (it: Var[_])     => variables += it
    case (it: Reactor[_]) => if (!levelQueue.contains(it)) levelQueue.offer(it)
  }

  private val variables: mutable.Set[Var[_]] = mutable.Set()
  private val levelQueue = new java.util.PriorityQueue[Reactor[_]](11, (x,y) => x.level - y.level)

  private[drx] def loop(): Unit = {
    println("var " + variables.map(_.id))
    variables.flatMap(_.getOuts).foreach(markDirty)
    variables.clear()
    var head: Reactor[_] = null
    while ({head = levelQueue.poll; head != null}) {
      val lvl = qts(levelQueue); println(head.level +" "+ head.id + " of " + lvl.map(_.level))
      // reeval may throw RetryLater, or else it will return true if its value changed
      head.reeval()
    }
    println()
    if (variables.nonEmpty) loop()
  }

  private def qts[T](queue: java.util.PriorityQueue[T]): Set[T] =
      scala.collection.JavaConverters.asScalaIterator(queue.iterator()).toSet
}


//      if (lvl.exists(_.level < head.level)) throw new RuntimeException("did not get smallest element :( " + head.level + " of " + lvl.map(_.level).toList)
