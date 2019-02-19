package drx

//import java.lang.ref.Cleaner

import upickle.default._

import scala.collection.mutable
import scala.util.{Success, Try}

object Var {
//  private val cleaner = Cleaner.create()

  def apply[X](buffer: X)(implicit n: Name): Var[X] =
    new Var(Success(buffer))(n)

  def mkEmptyVar[X](implicit n: Name): Var[X] =
    new Var[X](internals.emptyValExc())(n)
}

object SeqVar {
  def apply[X]()(implicit n: Name): SeqVar[X] =
    new SeqVar()(n)
}

// Distributed Programming: Building Functional Refuges inside Imperative Chaos

sealed class Var[X] private[drx](protected var buffer: Try[X])(implicit n: Name)
  extends DynamicRx[X](true, n.toString, buffer) with Rx[X] {
  // TODO watch that ad-hoc classes do not capture this of outer,
  //      bc Proxys finalize would no longer work

  override protected[this] val formula: Try[X] => X = _ => buffer.get

  forceObserved = true

  def set(newValue: X): Unit = if (newValue != buffer) {
    buffer = Success(newValue)
    withInstant(_.markSource(this))
  }

  // using the above definitions

  def transform(transformer: X => X): Unit = set(transformer(getIt.get))

//  // make observers and sources declarative by collecting them if unreachable
//  // turn all Sources & Sinks (Folds+Observers)
//  // into proxies/wrappers that only weakref the real Rx.
//  // Then we can kill/stop them in the finalizer.
//  override def finalize(): Unit = { rxit.freeze() }
}

sealed class SeqVar[X] private[drx](protected val buffer: mutable.Buffer[X] = mutable.Buffer[X]())(implicit n: Name)
  extends DynamicRx[Seq[X]](
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

// TODO parallel
// each change may start its own transaction. globally lock write levels.
// next transactions may begin running on levels that are lower than still
// running past transactions.

// TODO time travel
// save serializable list of transactions

//  // TODO async call value
//object FutureWork {
//  def async[X,Y](rx: Rx[X], init: Y, f: (X)=>Y): Rx[Y] = {
//    val input = rx.underlying.value
//    val result = new Var[Y](init)
//    runInThread { () =>
//      val value = f(input)
//      result.set(value)
//    }
//    result
//  }
//}
