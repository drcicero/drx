package drx

import scala.util.{Success, Try}

/* public methods for reactives. */
trait Rx[X] {
  private[drx] def underlying: InternalRx[X]
  def id: String

  //////////////////////////////////////////////////////////////////////////////
  // COMBINATORS

  /** create a dependent reactive. */
  def map[Y](func: X => Y, name: String = "")
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val result = new InternalRx[Y](underlying.remember, Signal.nameit("m",f,l)) with Rx[Y]
    result.formula = () => func(this.abstractget)
    result
  }

  /** turn reactive into a stream of changes. */
  def changes(name: String = "ch")
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](StreamKind, Signal.nameit(name,f,l)) with Rx[X]
    result.father = Some(this.underlying)
    withTransaction(_.runLater { () =>
      result.formula = () => this.abstractget
      result.observe {x => println("change " + x) }
    })
    result
  }

  /** turn reactive into a signal, with initial value. */
  def hold(init: X, name: String = "hd")
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] =
    fold(init)( (_, ev) => ev )(f,l)

  def mkFold[Y](init: Y)(comb: (Y, X) => Y)
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val result = new InternalRx[Y](SignalKind, Signal.nameit("f",f,l)) with Rx[Y] //with Sink[Y]
    result.value = Success(init)
    result.formula = () => {
      val tmp = this.abstractget
      comb(result.value.get, tmp)
    }
    result
    // (..., owner: VarLike = internals.dummyOwner)
    // internals.checksafety(this, owner)
  }

  /** create a stateful signal, that gets its incoming value and its previous value
    * to create the next value. fails inside a signal. */
  def fold[Y](init: Y)(comb: (Y, X) => Y)
             (implicit f: sourcecode.File, l: sourcecode.Line): Rx[Y] = {
    val result = mkFold(init)(comb)(f, l)
    result.observe {x => println("fold " + x) }
    result
  }

  /** this creates an callback, that is deactivated. you can do this even running inside a signal.
    * then, later and outside of signals you can start the callback.
    * see the accompanying javafx and scalajshtml renderer for an example to use this.
    * see also [[mkObs]]. */
  def mkObs(callback: X => Unit, onStart: () => Unit = ()=>{})
           (implicit f: sourcecode.File, l: sourcecode.Line): Sink[Unit] = {
    val result = new InternalRx[Unit](StreamKind, Signal.nameit("o",f,l)) with Sink[Unit] {
      override def onstart(): Unit = onStart()
    }
    result.formula = () => {
      val tmp = this.abstractget
      withTransaction(_.runLater(() => callback(tmp)))
    }
    result
  }

  /** creates a callback, that immediately starts running on each change.
    * fails inside a signal. see also [[mkObs]]. */
  def observe(callback: X => Unit)
           (implicit f: sourcecode.File, l: sourcecode.Line): Sink[Unit] = {
    val result = mkObs(callback)(f, l)
    result.start()
    result
  }

  def snapshot(stream: Rx[_])
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](StreamKind, Signal.nameit("snap",f,l)) with Rx[X]
    result.formula = () => {
      stream.abstractget
      this.abstractget
    }
    result
  }

  //////////////////////////////////////////////////////////////////////////////
  // get, sample

  /** Returns the current value of this signal.
    * Calling .get on a stream will fail with an Exception.
    *
    * Only works inside of Signal(...) expressions,
    * where it will create a dependency from 'this' to the Signal(...), so that
    * the Signal(...) will reevaluate, when 'this' reevaluates.
    *
    * See also [[sample]], [[trySample]]. */
  def get: X = tryGet.get

  /** Returns the current value of a signal,
    * or raises a EmptryStream Exception for streams.
    *
    * Only works outside of Signal(...) expressions.
    * It does not create a dependency between any reactives.
    *
    * See also [[trySample]], [[get]]. */
  def sample: X = trySample.get

  /** Returns the value of the 'this' signal or none for streams.
    *
    * Only works inside of Signal(...) expressions,
    * where it will create a dependency from 'this' to the Signal(...), so that
    * the Signal(...) will reevaluate, when 'this' reevaluates.
    *
    * See also [[get]], [[trySample]], [[sample]]. */
  private def tryGet: Try[X] = {
    val activeRx = internals.activeRx.value.getOrElse(
      throw new RuntimeException("Do not get outside of Signal(...)! Hint: use .sample()"))
    if (/* activeRx.remember == SignalKind && */ underlying.remember == StreamKind)
      throw new RuntimeException("You cannot call .get on a stream. " +
        "Hint: Turn the the stream into a signal using '.hold'.")
    annotateTry(underlying.tryGetValue(outer = Some(activeRx)))
  }

  /** Returns the value of the 'this' signal or none for streams.
    *
    * Only works outside of Signal(...) expressions.
    * It does not create a dependency between any reactives.
    *
    * See also [[sample]], [[get]]. */
  def trySample: Try[X] = {
    if (internals.activeRx.value.isDefined)
      throw new RuntimeException("Do not sample inside of Signal(...)! Hint: use .get()")

    if (internals.withRetries) {
      var tmp: Try[X] = internals.TheEmptyStream
      val obs = this.mkObs(x => tmp = Success(x))
      withTransaction.activeCtx.withValue(None) { obs.start() }
      obs.stop()
      println("hello " + tmp)
      tmp
    } else
      annotateTry(underlying.tryGetValue(outer = None))
//      annotateTry(withTransaction.activeCtx.withValue(None){withTransaction(underlying)(_.markRx(underlying))})
  }

  private[drx] def abstractget: X = {
    val activeRx = internals.activeRx.value.get // should be impossible to fail
    underlying.tryGetValue(outer = Some(activeRx)).get
  }

  /** debugging */
  def debugGetDirtyValue: Try[X] = underlying.value

  private def annotateTry[X](theTry: Try[X]): Try[X] =
    theTry.recover { case e =>
      val result = new EmptyStream(e)
      result.setStackTrace(result.getStackTrace.drop(4).filter(x => !x.getClassName.startsWith("drx.")).take(1))
      throw result
    }
}

//////////////////////////////////////////////////////////////////////////////
// Sinks

trait Sink[X] extends InternalRx[X] {
  def onstart(): Unit = {}

  private[drx] override def freeze(): Unit = {
    stop()
    super.freeze()
  }

//  def isForced(): Boolean = forceActive

  def start(): Unit = {
    if (internals.activeRx.value.isDefined) throw new RuntimeException(
      "You are starting an observer / creating a fold inside a signal. " +
      "That may lead to memory leaks.")

    if (!forceActive) {
      println("  o " + id)
      onstart()
      forceActive = true
      withTransaction(_.markRx(this))
    }
  }

  def stop(): Unit = if (forceActive) {
    println("  u " + id)

    forceActive = false
    getIns.foreach(_.unpushto(this))
  }
}

//////////////////////////////////////////////////////////////////////////////
// Signal Expressions

/** create a dynamic signal. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Signal {
  def apply[X](formula: => X, name: String = "")
           (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](SignalKind, nameit(name,f,l)) with Rx[X]
    result.formula = formula _
    result
  }

  private[drx] def nameit(s: String, f: sourcecode.File, l: sourcecode.Line) = {
    val ff = f.value.substring(f.value.lastIndexOf("/")+1)
    val fff = ff.substring(0, ff.indexOf("."))
    s"${s}_$fff:${l.value}"
  }
}
