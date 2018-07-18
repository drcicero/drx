package drx

import scala.util.{Success, Try}

/* public methods for reactives. */
trait Rx[X] {
  private[drx] def underlying: InternalRx[X]
  def id: String

  //////////////////////////////////////////////////////////////////////////////
  // COMBINATORS

  /** create a dependent reactive. */
  def map[Y](func: X => Y, name: String = ""): Rx[Y] = {
    val result = new InternalRx[Y](underlying.remember, "m") with Rx[Y]
    result.formula = () => func(this.abstractget)
    result
  }

  /** turn reactive into a stream of changes. */
  def changes(name: String = "ch"): Rx[X] = {
    val result = new InternalRx[X](StreamKind, name) with Rx[X]
    result.formula = () => this.abstractget
    result
  }

  /** turn reactive into a signal, with initial value. */
  def hold(init: X, name: String = "hd"): Rx[X] =
    fold(init)( (_, ev) => ev )

  def mkFold[Y](init: Y)(comb: (Y, X) => Y): Rx[Y] with Sink[Y] = {
    val result = new InternalRx[Y](SignalKind, "f") with Rx[Y] with Sink[Y]
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
  def fold[Y](init: Y)(comb: (Y, X) => Y): Rx[Y] = {
    val result = mkFold(init)(comb)
    result.start()
    result
  }

  /** this creates an callback, that is deactivated. you can do this even running inside a signal.
    * then, later and outside of signals you can start the callback.
    * see the accompanying javafx and scalajshtml renderer for an example to use this.
    * see also [[mkObs]]. */
  def mkObs(callback: X => Unit): Sink[Unit] = {
    val result = new InternalRx[Unit](StreamKind, "o") with Sink[Unit]
    result.formula = () => {
      val tmp = this.abstractget
      withTransaction(_.runLater(() => callback(tmp)))
    }
    result
  }

  /** creates a callback, that immediately starts running on each change.
    * fails inside a signal. see also [[mkObs]]. */
  def observe(callback: X => Unit): Sink[Unit] = {
    val result = mkObs(callback)
    result.start()
    result
  }

  def snapshot(stream: Rx[_]): Rx[X] = {
    val result = new InternalRx[X](StreamKind, "") with Rx[X]
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
  private[drx] override def freeze(): Unit = {
    stop()
    super.freeze()
  }

  def start(): Unit = {
    if (internals.activeRx.value.isDefined) throw new RuntimeException(
      "You are starting an observer / creating a fold inside a signal. " +
      "That may lead to memory leaks.")

    if (!forceActive) {
      forceActive = true
      withTransaction(_.markRx(this))
    }
  }

  def stop(): Unit = if (forceActive) {
    forceActive = false
    getIns.foreach(_.unpushto(this))
  }
}

//////////////////////////////////////////////////////////////////////////////
// Signal Expressions

/** create a dynamic signal. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Signal {
  def apply[X](formula: => X, name: String = ""): Rx[X] = {
    val result = new InternalRx[X](SignalKind, name) with Rx[X]
    result.formula = formula _
    result
  }
}
