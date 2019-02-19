package drx

import scala.util.{Success, Try}

//object TypeMath {
//  type Fn[X,Y] = X => Y             // Proc // Can Return Different Values! Can Fail! Can Block/Take Long Time!
//  def apply[X,Y](c: Fn[X,Y]): Y = c()
//
//  type Recv[T]   = Fn[Unit,T]       // Get  // Stdin.readln; Point.getX; Channel.recv; Future.await, Iterable.next()
//  type Send[T]   = Fn[T,Unit]       // Set  // Stdout.println; Point.setX; Channel.send; Future.new
//
//  def foreach[T](g: Recv[T], s: Send[T]): Unit = fork {try{ while (true) s(g()) } catch { case NoSuchElementException => }}
//  def mkProc[X,Y](s: Send[X], g: Recv[Y]): Fn[X, Y] = x => g(s(x))
//
//  def mkEvt[T](s: Send[T], g: Recv[T]): Evt   = (_:Unit) => s(g())
//  type Evt       = Fn[Unit,Unit]
//  def fire(c: Fn[Unit,Unit]): Unit = c()
//
//  type Queue[T]  = (Recv[T], Send[T]) // Var  // Property, Queue, Promise
//
//  type GetVar[T]   =Recv[Queue[T]]    // Property.create, Queue.create, Promise.create
//  type GetCoro[X,Y]=Recv[Fn[X,Y]] // coro.create
//
//  // a future has a send[send[x]] // called then
//  // a future has a recv[x]       // called await
//
//  def then_[X](ss: Send[Send[X]], s: Send[X]): Unit = ss(s) // obs.register; future.then; list.foreach
//  def map[X,Y](ss: Send[Send[X]], f: Fn[X,Y]): Send[Send[X]] = ss(f) // obs.register; future.then; list.foreach
//  def iter[T](gg: Recv[Recv[T]], s: Send[T]): Unit = foreach(gg(), s)  // let it = List.iterator(); it.next(); it.next()
//  def x[T]: Recv[Recv[Send[Send[T]]]] = ???
//
//  type XXX[T]   = Get[Set[T]]   //
//  type YYY[T]   = Set[Get[T]]   //
//}

private[drx] trait Getr[+T] {
  private[drx] def getValue: T
//  def parent: FromRx[T]
}

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
    new DynamicRx[Y](true, n.toString, Success(init)) with Rx[Y] {
      override protected[this] val formula: Try[Y] => Y = {x => comb(x.get, that.get)}
    }
  }

    /** create a stateful signal, that gets its incoming value and its previous value
    * to create the next value. fails inside a signal. */
  @inline def scan[Y](init: Y)(comb: (Y, X) => Y)(implicit n: Name): Rx[Y] = {
    val that = this
    val result = new DynamicRx[Y](true, n.toString, Success(init)) with Rx[Y] {
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

//////////////////////////////////////////////////////////////////////////////
// Sinks

class Obs[+X] private[drx](in: Rx[X],
                                    onNext: X => Unit,
                                    onError: Throwable => Unit, n: Name)
  extends DynamicRx[Unit](false, n.toString) {

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
