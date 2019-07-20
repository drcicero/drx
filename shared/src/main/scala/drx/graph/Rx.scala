package drx.graph

import drx.internals.EmptyValExc
import drx.{Name, graph, internals, withInstant}

import scala.util.{Success, Try}

private[drx] object Rx {
  def apply[X](func: () => X/*, theParent: FromRx[X]*/)(name: String)(implicit n: Name): Rx[X] =
    new Getr[X] with Rx[X] {
      override def toString: String = name
      override def getValue: X = func()
      //      override def parent: FromRx[X] = theParent
    }
}

/* public methods for reactives. */
trait Rx[+X] { this: Getr[X] =>
  def sample: X = get

  //////////////////////////////////////////////////////////////////////////////
  // Strict

  @inline def mkScan[Y](init: Y)(comb: (Y, X) => Y)(implicit n: Name): Rx[Y] = {
    val that = this
    new RxDynamic[Y](true, n.toString, Success(init)) with Rx[Y] {
      override protected[this] val formula: Try[Y] => Y = {x => comb(x.get, that.get)}
    }
  }

  /** create a stateful signal, that gets its incoming value and its previous value
    * to create the next value. fails inside a signal. */
  @inline def scan[Y](init: Y)(comb: (Y, X) => Y)(implicit n: Name): Rx[Y] = {
    val that = this
    val result = new RxDynamic[Y](true, n.toString, Success(init)) with Rx[Y] {
      override protected[this] val formula: Try[Y] => Y = {x => comb(x.get, that.get)}
    }
    result.start()
    result
  }

  // TODO differentiate history aware foreach, from signal foreach
  //      need to write history aware foreach.
  //      make history aware foreach default.
  //      signal foreach is for graphics?

  /** this creates an callback, that is deactivated. you can do this even inside a signal.
    * then, later and outside of signals you can start the callback.
    * see the accompanying javafx and scalajshtml renderer for an example to use this.
    * see also [[mkForeach]]. */
  @inline def mkForeach[Y >: X](onNext: Y => Unit,
                                onError: Throwable => Unit = {throw _})(implicit n: Name): Obs[X] = {
    new Obs[X](this, onNext, onError, n)
  }

  /** creates a callback, that immediately starts running on each change.
    * fails inside a signal, see also [[mkForeach]]. */
  @inline def foreach(onNext: X => Unit,
                      onError: Throwable => Unit = {throw _})(implicit n: Name): Obs[X] = {
    val result = new Obs[X](this, onNext, onError, n)
    result.start()
    result
  }

  //////////////////////////////////////////////////////////////////////////////
  // COMBINATORS

  /** create a dependent reactive. */
  @inline def map[Y](func: X => Y)(implicit n: Name): Rx[Y] =
    Rx[Y](() => func(this.get))(n.toString)

  @inline def filter(func: X => Boolean)(implicit n: Name): Rx[X] =
    Rx[X]({ () =>
      val x = this.get
      if (func(x)) x else throw internals.emptyValExc().get
    })(n.toString)

  @inline def changes(implicit n: Name): Rx[X] = {
    var oldValue: Try[X] = internals.emptyValExc()
    Rx[X]{ () =>
      val newValue = Try(this.get)
      val result =
        if (newValue != oldValue) newValue
        else internals.emptyValExc()
      oldValue = newValue
      result.get
    }(n.toString)
  }

  @inline def hold[Y >: X](init: Y)(implicit n: Name): Rx[Y] = {
    var oldValue: Try[Y] = Success(init)
    Rx[Y]{ () =>
      val newValue = Try(this.get)
      if (newValue.isFailure && newValue.failed.get.isInstanceOf[EmptyValExc])
        oldValue.get
      else {
        oldValue = newValue
        oldValue.get
      }
    }(n.toString)
  }

  @inline def zip[Y](rx2: Rx[Y])(implicit n: Name): Rx[(X,Y)] =
    Rx[(X,Y)](() => (this.get, rx2.get))(n.toString)

  //  // TODO? currently fires on both changes and returns only the second... thats not 'snapshot'.
  //  //       this is just rx.zip.map(._2)
  //  @inline def snapshot(stream: Rx[_])
  //                      (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
  //    val result = new InternalRx[X](Val.nameit("snap",f,l)) with Rx[X]
  //    result.formula = () => {
  //      stream.annotateGet
  //      this.annotateGet
  //    }
  //    result
  //  }

  //////////////////////////////////////////////////////////////////////////////
  // get, sample

  /** Returns the current value of a rx, or raises a [[EmptyValExc]].
    *
    * If used inside a Dynamic Expression ([[Val]], [[Scan]], ...),
    * it will create a dependency from 'this' to the Expression, so that
    * the Signal(...) will reevaluate, when 'this' reevaluates.
    *
    * If used outside a Dynamic Expression,
    * it will just return the current value. */
  @inline def get: X = {
    if (internals.activeRx.value.isDefined && internals.withRetries) {
      var tmp: Option[X] = None
      val obs = this.mkForeach(x => tmp = Some(x))
      withInstant.activeInstant.withValue(None) { obs.start() }
      obs.stop()
      tmp.getOrElse(throw internals.emptyValExc().get)
    } else
      getValue
  }

}
