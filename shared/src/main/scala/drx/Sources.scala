package drx

import scala.util.Success

trait VarLike extends AnyRef {
  private[drx] def getEventsources: Set[EventSource[_]]
}

abstract class EventSource[X](remember: Remember, name: String)
  extends ProxyRx[X](remember, name) with VarLike {
  underlying.forceActive = true
}

class Channel[X](name: String = "") extends EventSource[X](StreamKind, name) {
  def send(newValue: X): Unit = {
    val real = underlying
    real.formula = () => newValue
    withTransaction { tx =>
      tx.markSource(real)
      tx.runLater { () =>
        real.value = internals.TheEmptyStream
        real.formula = () => internals.TheEmptyStream.get
      }
    }
  }
  private[drx] def getEventsources: Set[EventSource[_]] = Set(this)
}

class Variable[X](init: X, name: String = "") extends EventSource[X](SignalKind, name) {
  underlying.value = Success(init)
  underlying.formula = () => init
  def set(newValue: X): Unit = {
    underlying.formula = () => newValue
    withTransaction(_.markSource(underlying))
  }
  def transform(transformer: X => X): Unit = set(transformer(underlying.value.get))
  private[drx] def getEventsources: Set[EventSource[_]] = Set(this)
}

// TODO make observers and sources declarative by collecting them if unreachable
// turn all Sources & Sinks (Folds+Observers)
// into proxies/wrappers that only weakref the real Rx.
// Then we can kill/stop them in the finalizer.
class ProxyRx[X](isEvent: Remember, name: String) extends Rx[X] {
  private[drx] var underlying: InternalRx[X] = new InternalRx[X](isEvent, name)
  override def id: String = underlying.id
  override def finalize(): Unit = { underlying.freeze(); underlying = null }
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
//    val result = new Variable[Y](init)
//    runInThread { () =>
//      val value = f(input)
//      result.set(value)
//    }
//    result
//  }
//}

// CHECK propagate errors
// turn value from Option[X] to Either[X, RuntimeException]
