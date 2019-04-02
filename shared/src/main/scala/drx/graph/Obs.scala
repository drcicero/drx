package drx.graph

import drx.internals.EmptyValExc
import drx.{Name, withInstant}

import scala.util.Try

class Obs[+X] private[drx](in: Rx[X],
                           onNext: X => Unit,
                           onError: Throwable => Unit, n: Name)
  extends DynamicRx[Unit](true, n.toString) {

  override def start(): Unit = super.start()
  override def stop(): Unit = super.stop()

  override protected[this] val formula: Try[Unit] => Unit = { _ =>

    try {
      val x = in.get
      withInstant(_.runLater(() => onNext(x)))
    } catch {
      case _: EmptyValExc =>
      case e: Throwable => withInstant(_.runLater(() => onError(e)))
    }

  }
}

//  override private[drx] def freeze(): Unit = {
//    stop()
//    onFreeze()
//  }
