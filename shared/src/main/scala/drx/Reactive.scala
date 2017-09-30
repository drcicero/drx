package drx

/* public methods for reactives. */
class Rx[X](isEvent: Boolean, name: String) extends InternalRx[X](isEvent, name) {

  /** create a stateful signal, that gets its incoming value and its previous value
    * to create the next value. fails inside a signal. */
  def fold[Y](init: Y, owner: VarLike = internals.dummyOwner)(comb: (Y, X) => Y): Rx[Y] = {
    val result = new Rx[Y](false, "f") with Sink[Y]
    result.father = Some(this)
    result.value = Some(init)
    result.formula = () => {
      val tmp = this.get
      comb(result.value.get, tmp)
    }
    result.start()
    result
  }

  /** this creates an callback, that is deactivated. you can do this even running inside a signal.
    * then, later and outside of signals you can start the callback.
    * see the accompanying javafx and scalajshtml renderer for an example to use this.
    * see also [[mkObs]]. */
  def mkObs(callback: X => Unit): Sink[Unit] = {
    val result = new InternalRx[Unit](true, "o") with Sink[Unit]
    result.father = Some(this)
    result.formula = () => {
      val tmp = this.get
      withContext(_.runLater(() => callback(tmp)))
    }
    result
  }

  /** creates a callback, that immediately starts running on each change.
    * fails inside a signal. see also [[mkObs]]. */
  def observe(callback: X => Unit): Sink[Unit] = {
    val result = new InternalRx[Unit](true, "o") with Sink[Unit]
    result.father = Some(this)
    result.formula = () => {
      val tmp = this.get
      withContext(_.runLater(() => callback(tmp)))
    }
    result.start()
    result
  }

  /** create a dependent reactive. */
  def map[Y](func: X => Y, name: String = ""): Rx[Y] = {
    val result = new Rx[Y](this.isEvent, "m")
    result.father = Some(this)
    result.formula = () => func(this.get)
    result
  }

  /** if inside a signal, read another signal, and create a dependency. */
  def get: X = {
    val rxmark = internals.activeRx.value.get

    // if enclosing rx is needed, ensure pushing and return value
    if (rxmark.rx.isNeeded) {
      pushto(rxmark.rx, rxmark.markOuts)
      return value.getOrElse(throw EmptyStream)
    }

    // else we are pulling
    if (isNeeded) // if this is needed, value is up to date and can simply be returned
      value.getOrElse(throw EmptyStream)
    else // if not, we need to calculate the value
      reeval(markOuts = false)
  }

  /** try to read the current value of a signal, if it has one.
    * if the value is observed elsewhere, it will simply look the the value up,
    * else it will calculate the value.
    * You cannot sample a value running inside a signal. */
  def sample: X = {
    // TODO maybe we can allow this? reeval may create another context, would that be bad?
    if (internals.activeRx.value.isDefined)
      throw new RuntimeException("Do not sample inside of signals.")
    if (withContext.activeCtx.value.isDefined)
      throw new RuntimeException("Do not sample inside of transactions.")

    // if this isNeeded then the value is up to date and can simply be returned,
    // else we have to calculate the value
    if (this.isNeeded)
      value.getOrElse(throw EmptyStream)
    else
      reeval(markOuts = false)
  }

  def changes(name: String = ""): Rx[X] = {
    val result = new Rx[X](true, "m")
    result.father = Some(this)
    result.formula = () => this.get
    result
  }

  def hold(init: X, name: String = ""): Rx[X] = {
    val result = new Rx[X](false, "m")
    result.father = Some(this)
    result.value = Some(init)
    result.formula = () => this.get
    result
  }
}

//object Flattener {
//  implicit class RxRx[X](ss: Rx[Rx[X]]) {
//    def flatten(name: String = ""): Rx[X] = {
//      // TODO if father were to be changed (flatten/switch?), it potentially must be removed from to
//      val result = new Rx[X](ss.isEvent, name)
//      ss.map(s => s.map { x =>
//        result.value = Some(x)
//        withContext(_.markSignal(result))
//      })
//      result
//    }
//  }
//}

trait Sink[X] extends InternalRx[X] {
//  private[drx] override def kill(): Unit = { stop(); super.kill() }
  def start(): Unit = if (!forceNeeded) {
    if (internals.activeRx.value.isDefined) throw new RuntimeException(
      "You are starting an observer / creating a fold inside a signal. " +
      "That very well may lead to memory leaks.")
    forceNeeded = true
//    father.foreach(_.pushto(this, markOuts = true))
    withContext(_.markRx(this, markOuts = true))
  }
  def stop(): Unit = if (forceNeeded) {
    forceNeeded = false
    (father ++ ins).foreach(_.unpushto(this))
  }
}

/** create a dynamic signal. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Signal {
  def apply[X](formula: => X, name: String = ""): Rx[X] = {
    val result = new Rx[X](true, name)
    result.formula = formula _
    result
  }
}

// TODO Future work: make
//   Rx.*
// return proxies instead, so we get accurate info over finalizers, which nodes
// are still used.
