package drx

import scala.collection.mutable
import scala.util.Success
import scala.util.Try

/** create using [[VarOwner.mkStore]] */
sealed class Store[X <: VarLike, CtorArgs](ctor: CtorArgs => X, name: String = "")
  extends EventSource[Seq[X]](SignalKind, name) {

  underlying.value = Success(Seq[X]())
//  val diffs = new Channel[(Set[X], Set[X])]()
  val diffs = this.fold((Set[X](),(Set[X](), Set[X]())))((state, event) => {
    (event.toSet, (state._1--event, event.toSet--state._1))
  }).map(_._2).changes()
//  diffs.observe { it => println("change " + it ) }
//  diffs.observe { x =>
//    println("ok")
//    try println(x) catch {case e:Throwable => e.printStackTrace}
//  }

  def update(plus: Iterable[CtorArgs], minus: Iterable[X]): Iterable[X] = {
    var vari: Iterable[X] = Seq()
    val minus_ = minus.toSeq
    vari = plus.map(ctor)
    underlying.formula = () => underlying.value.get.filter(!minus_.contains(_)) ++ vari
    withTransaction(_.markSource(underlying))
    minus_.flatMap(_.getEventsources).foreach(_.underlying.freeze())
    vari
  }

  def creates(args: Iterable[CtorArgs]): Iterable[X] = update(args, None)
  def create(args: CtorArgs): X = update(Some(args), None).toSeq.apply(0)
  def remove(minus: Iterable[X]): Unit = update(None, minus)
  override def fire(newValue: Seq[X]): Unit = {
    underlying.formula = () => underlying.value.get ++ newValue
    withTransaction(_.markSource(underlying))
  }

//  def diffFold[Y](init: Y)(func: (Y,Set[X],Set[X]) => Y): Rx[Y] =
//    diffs.fold[Y](init)((state, event) => func(state, event._1, event._2))

//  /** only do this if func is expensive. */
//  def mapmap[Y](func: X => Y): Rx[List[Y]] = {
//    val last = mutable.Map[X,Y]()
//    val real = underlying
//    diffs.map { it =>
//      val (minus, plus) = it
//      last --= minus
//      last ++= plus.zip(plus.map(func))
//      last.values.toList.sortBy(real.value.get.indexOf(_))
//    }
//  }

  override /*private[drx]*/ def getEventsources: Seq[EventSource[_]] =
    underlying.value.get.flatMap(_.getEventsources)
}

trait VarOwner extends VarLike {
  protected val children: mutable.ListBuffer[VarLike] = mutable.ListBuffer()

  def mkSource[X](name: String = ""): Channel[X] = {
    val result = new Channel[X](name)
    children += result
    result
  }

  def mkVar[X](init: X, name: String = ""): Variable[X] = {
    val result = new Variable(init, name)
    children += result
    result
  }

  def mkStore[X <: VarLike, CtorArgs](ctor: CtorArgs => X, name: String = ""): Store[X, CtorArgs] = {
    val result = new Store(ctor, name)
    children += result
    result
  }

  /*private[drx]*/ def getEventsources: Seq[EventSource[_]] =
    children.toSeq.flatMap(_.getEventsources)
}
