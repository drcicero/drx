package drx.pull

import drx.interface.{Bag, BagBuilder, DSLTrait}
import drx.interface.DSL._

import scala.collection.mutable

object pullDSL extends DSLTrait {
  private[pull] val dirtyVars = mutable.Set[PRawVar[_,_]]()
  private[pull] val enableds = mutable.Set[Val[_]]()

  override def RawVar[I,O](init: Bag[I], f: Bag[I] => O): RawVar[I,O] = new PRawVar[I,O](init, f)
  override def Val[X](e: => X) = new PVal(e _)

  override protected[drx] def forceStep(): Unit = {
    println("tick " + dirtyVars)

    dirtyVars.foreach(_.clean())
    dirtyVars.clear()

    var runo = mutable.Set[Val[_]]()
    var newo = ((enableds ++ getEnableds.flatMap(_())) -- runo).toSet
    while (newo.nonEmpty) {
      //println("  obs " + newo)
      newo.foreach(_.sample)
      runo ++= newo
      newo = ((enableds ++ getEnableds.flatMap(_())) -- runo).toSet
    }

    println("  end " + enableds.size)
  }
}

final class PVal[+X](e: () => X) extends Val[X](pullDSL) {
  override def get: X = e()
  override def sample: X = e()
  override def enable(): Unit = atomic { pullDSL.enableds += this }
  override def disable(): Unit = pullDSL.enableds -= this
}

final class PRawVar[I,O](init: Bag[I], reduce: Bag[I] => O) extends RawVar[I,O](reduce, pullDSL) {
  private var curr = init
  private val next: BagBuilder[I] = new BagBuilder[I]()

  override def get: O = reduce(curr)
  override def sample: O = reduce(curr)
  override def enable(): Unit = atomic { pullDSL.enableds += this }
  override def disable(): Unit = pullDSL.enableds -= this
  override def update(newValue: Bag[I]): Unit = { next ++= newValue; atomic(pullDSL.dirtyVars += this) }

  protected[pull] def clean(): Unit = {
    curr = next.result()
    next.clear()
  }
}
