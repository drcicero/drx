package standard.pull

import standard.interface.DSL
import standard.interface.DSL._

import scala.collection.mutable

object pullDSL extends DSL {
  override def Val[X](e: => X) = new SVal(e _)
  override def Var[X](e: => X) = new SVar(e _)
  override def MultiVar[X]() = new SMultiVar[X]()

  private[pull] val dirtyVars = mutable.Set[Touchable]()
  private[pull] val forceObs = mutable.Set[Val[_]]()

  override def forceTick(): Unit = {
    println("tick " + dirtyVars)

    dirtyVars.foreach(_.touch())
    dirtyVars.clear()

    var runo = mutable.Set[Val[_]]()
    var newo = (forceObs -- runo).toSet
    while (newo.nonEmpty) {
      println("  obs " + newo)
      newo.foreach(_.sample)
      runo ++= newo
      newo = (forceObs -- runo).toSet
    }

    println("  end")
  }
}

final class SVal[+X](e: () => X) extends Val[X](pullDSL) {
  override def get: X = { val x = e(); println("    get " + x); x }
  override def sample: X = e()
  override def forceStart(): Unit = { pullDSL.forceObs += this }
  override def forceStop(): Unit = { pullDSL.forceObs -= this }
}

trait Touchable {
  protected[pull] def touch(): Unit
}

final class SVar[X](start: () => X) extends Var[X](pullDSL) with Touchable {
  private var curr: () => X = start
  private var next: () => X = start
  override def get: X = curr()
  override def sample: X = curr()
  override def forceStart(): Unit = { pullDSL.forceObs += this }
  override def forceStop(): Unit = { pullDSL.forceObs -= this }
  override def set(e: => X): Unit = { next = e _; pullDSL.transact(pullDSL.dirtyVars += this) }
  override protected[pull] def touch(): Unit = { println("  set" + (curr, next)); curr = next }
}

final class SMultiVar[X] extends MultiVar[X](pullDSL) with Touchable {
  private var curr = Seq[X]()
  private val next: mutable.Buffer[X] = mutable.Buffer[X]()
  override def get: Seq[X] = curr
  override def sample: Seq[X] = curr
  override def forceStart(): Unit = { pullDSL.forceObs += this }
  override def forceStop(): Unit = { pullDSL.forceObs -= this }
  override def set(newValue: X): Unit = { next ++= Seq(newValue); pullDSL.transact(pullDSL.dirtyVars += this) }
  override protected[pull] def touch(): Unit = { println("  mset " + (curr, next)); val tmp = Seq() ++ next; next.clear(); curr = tmp }
}
