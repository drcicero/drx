package drx.pull

import drx.interface.DSL
import drx.interface.DSL._

import scala.collection.mutable

object pullDSL extends DSL {
  private[pull] val dirtyVars = mutable.Set[Dirty]()
  private[pull] val forceObs = mutable.Set[Val[_]]()

  override def RawVar[I,O](init: Seq[I], f: Seq[I] => O): RawVar[I,O] = new PRawVar[I,O](init, f)
  override def Val[X](e: => X) = new PVal(e _)

  override protected def forceStep(): Unit = {
    println("tick " + dirtyVars)

    dirtyVars.foreach(_.clean())
    dirtyVars.clear()

    var runo = mutable.Set[Val[_]]()
    var newo = (forceObs -- runo).toSet
    while (newo.nonEmpty) {
      //println("  obs " + newo)
      newo.foreach(_.sample)
      runo ++= newo
      newo = (forceObs -- runo).toSet
    }

    println("  end " + forceObs.size)
  }

  private[pull] def scan[X,Y](that: Val[X], init: Y, f: (Y, X) => Y): Val[Y] = {
    var x: X = that.get
    var y: Y = init
    Val { val newx = that.get; if (x != newx) { x = newx; y = f(y, x) }; y }
  }
}

final class PVal[+X](e: () => X) extends Val[X](pullDSL) {
  override def get: X = e()
  override def sample: X = e()
  override def scan[Y](init: Y)(f: (Y, X) => Y): Val[Y] = pullDSL.scan(this, init, f)
  override def enable(): Unit = transact { pullDSL.forceObs += this }
  override def disable(): Unit = transact { pullDSL.forceObs -= this }
}

trait Dirty {
  protected[pull] def clean(): Unit
}

final class PRawVar[I,O](init: Seq[I], reduce: Seq[I] => O) extends RawVar[I,O](reduce, pullDSL) with Dirty {
  private var curr = init
  private val next: mutable.Buffer[I] = mutable.Buffer[I]()

  override def get: O = reduce(curr)
  override def sample: O = reduce(curr)
  override def scan[Y](init: Y)(f: (Y, O) => Y): Val[Y] = pullDSL.scan(this, init, f)
  override def enable(): Unit = transact { pullDSL.forceObs += this }
  override def disable(): Unit = transact { pullDSL.forceObs -= this }
  override def set(newValue: I): Unit = { next ++= Seq(newValue); transact(pullDSL.dirtyVars += this) }
  override protected[pull] def clean(): Unit = {
    //println("  mset " + (curr, next))
    val tmp = Seq() ++ next
    next.clear()
    curr = tmp
  }
}