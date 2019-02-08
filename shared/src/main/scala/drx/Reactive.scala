package drx

import scala.util.{Failure, Success, Try}

private[drx] trait Observable[T] {
  private[drx] def getValue: T
}

/* public methods for reactives. */
trait Rx[X] {
  private[drx] def underlying: Observable[X]
  def id: String

  //////////////////////////////////////////////////////////////////////////////
  // COMBINATORS

  /** create a dependent reactive. */
  @inline def map[Y](func: X => Y, name: String = "")
            (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val that = this
    new Rx[Y] {
      override val id: String = Val.nameit(name,f,l)
      override val underlying: Observable[Y] = new Observable[Y] {
        override def getValue: Y = func(that.underlying.getValue)
      }
    }
  }

  @inline def filter(func: X => Boolean, name: String = "")
            (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](Val.nameit("m", f, l)) with Rx[X]
    result.formula = { () =>
      val x = this.underlying.getValue
      if (func(x)) x else throw internals.emptyValExc().get
    }
    result
  }

  @inline def mkScan[Y](init: Y, name: String = "f")(comb: (Y, X) => Y)
                       (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val result = new InternalRx[Y](Val.nameit(name,f,l)) with Rx[Y]
    result.remember = true
    result.value = Success(init)
    result.formula = () => {
      comb(result.value.get, this.underlying.getValue)}
    result
    // (..., owner: VarLike = internals.dummyOwner)
    // internals.checksafety(this, owner)
  }

  /** create a stateful signal, that gets its incoming value and its previous value
    * to create the next value. fails inside a signal. */
  @inline def scan[Y](init: Y, name: String = "f")(comb: (Y, X) => Y)
                     (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val result = mkScan(init, name)(comb)(f, l)
    result.observe {_ => }
    result
  }

  /** this creates an callback, that is deactivated. you can do this even inside a signal.
    * then, later and outside of signals you can start the callback.
    * see the accompanying javafx and scalajshtml renderer for an example to use this.
    * see also [[mkObs]]. */
  @inline def mkObs(onNext: X => Unit,
                    onStart: () => Unit = ()=>{},
                    onError: Throwable => Unit = {throw _})
           (implicit f: sourcecode.File, l: sourcecode.Line): Obs[Unit] = {
    val result = new InternalRx[Unit](Val.nameit("o",f,l)) with Obs[Unit] {
      override def onstart(): Unit = onStart()
    }
    result.formula = () => {
      val tmp = Try(underlying.getValue)
      withInstant(_.runLater{ () => tmp match {
        case Success(value)     => onNext(value)
        case Failure(exception) => onError(exception)
      }})
      throw internals.emptyValExc().get
    }
    result
  }

  /** creates a callback, that immediately starts running on each change.
    * fails inside a signal, see also [[mkObs]]. */
  @inline def observe(callback: X => Unit)
             (implicit f: sourcecode.File, l: sourcecode.Line): Obs[Unit] = {
    val result = mkObs(callback)(f, l)
    result.start()
    result
  }

  @inline def changes(name: String = "")
                     (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result: Rx[X] = new InternalRx[X](Val.nameit("m",f,l)) with Rx[X] {
      remember = true
      formula = () => value.get
    }
    result
  }

  @inline def hold(init: X, name: String = "")
                  (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result: Rx[X] = new InternalRx[X](Val.nameit("m",f,l)) with Rx[X] {
      value = Success(init)
      remember = true
      formula = () => value.get
    }
    result
  }

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

  /** Returns the current value of this signal.
    * Calling .get on a stream will fail with an Exception.
    *
    * Only works inside of Signal(...) expressions,
    * where it will create a dependency from 'this' to the Signal(...), so that
    * the Signal(...) will reevaluate, when 'this' reevaluates.
    *
    * See also [[sample]]. */
  @inline def get: X = {
    if (internals.activeRx.value.isEmpty)
      throw new RuntimeException("Do not get outside of Signal(...)! Hint: use .sample()")
    underlying.getValue
  }

  /** Returns the current value of a signal,
    * or raises a EmptryStream Exception for streams.
    *
    * Only works outside of Signal(...) expressions.
    * It does not create a dependency between any reactives.
    *
    * See also [[get]]. */
  @inline def sample: X = {
    if (internals.activeRx.value.isDefined)
      throw new RuntimeException("Do not sample inside of Signal(...)! Hint: use .get()")

    if (internals.withRetries) {
      var tmp: Option[X] = None
      val obs = this.mkObs(x => tmp = Some(x))
      withInstant.activeCtx.withValue(None) { obs.start() }
      obs.stop()
      tmp.getOrElse(throw internals.emptyValExc().get)
    } else
      underlying.getValue
  }

}

//////////////////////////////////////////////////////////////////////////////
// Sinks

trait Obs[X] extends InternalRx[X] {
  def onstart(): Unit = {}

  private[drx] override def freeze(): Unit = {
    stop()
    super.freeze()
  }

  def isForced: Boolean = forceActive

  def start(): Unit = {
    if (internals.activeRx.value.isDefined) throw new RuntimeException(
      "You are starting an observer / creating a fold inside a signal. " +
      "That may lead to memory leaks.")

    if (!forceActive) {
      onstart()
      forceActive = true
      withInstant(_.markRx(this))
    }
  }

  def stop(): Unit = if (forceActive) {
    forceActive = false
    getIns.foreach(_.unpushto(this))
  }
}
