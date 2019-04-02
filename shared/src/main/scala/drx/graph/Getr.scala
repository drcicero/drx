package drx.graph

private[drx] trait Getr[+T] {
  private[drx] def getValue: T
//  def parent: FromRx[T]
}

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
