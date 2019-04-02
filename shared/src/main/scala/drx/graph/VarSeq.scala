package drx.graph

import drx.{Name, withInstant}

import scala.collection.mutable
import scala.util.{Success, Try}

object VarSeq {
  def apply[X]()(implicit n: Name): VarSeq[X] =
    new VarSeq()(n)
}

sealed class VarSeq[X] private[drx](protected val buffer: mutable.Buffer[X] = mutable.Buffer[X]())(implicit n: Name)
  extends RxDynamic[Seq[X]](
    true, // actually false?
    n.toString,
    Success(Seq() ++ buffer)) with Rx[Seq[X]] {

  forceObserved = true

  def set(newValue: X): Unit = {
    buffer ++= Seq(newValue)
    withInstant(_.ensureSource(this))
  }

  def set(newValue: Seq[X]): Unit = if (newValue.nonEmpty) {
    buffer ++= newValue
    withInstant(_.ensureSource(this))
  }

  override protected[this] val formula: Try[Seq[X]] => Seq[X] =
    _ => { val tmp = Seq() ++ buffer; buffer.clear(); tmp }
}
