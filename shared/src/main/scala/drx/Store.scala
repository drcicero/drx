package drx

import scala.collection.mutable
import scala.util.Success
import scala.util.Try

/** create using [[VarOwner.mkStore]] */
sealed class Store[X <: VarLike, CtorArgs](ctor: CtorArgs => X, name: String = "")
  extends EventSource[Set[X]](SignalKind, name) {

  underlying.value = Success(Set[X]())
//  val diffs = new Channel[(Set[X], Set[X])]()
  val diffs = this.fold((Set[X](),(Set[X](), Set[X]())))((state, event) => {
    (event, (state._1--event, event--state._1))
  }).map(_._2)
//  diffs.observe { x =>
//    println("ok")
//    try println(x) catch {case e:Throwable => e.printStackTrace}
//  }

  def creates(args: Iterable[CtorArgs]): Iterable[X] = {
    var vari: Iterable[X] = Seq()
    withTransaction { tx =>
      vari = args.map(ctor)
      underlying.formula = () => underlying.value.get ++ vari
      tx.markSource(underlying)
    }
//    diffs.send((Set(), vari.toSet))
    vari
  }

  def create(args: CtorArgs): X = creates(Some(args)).toSeq.apply(0)

  def remove(minus: Iterable[X]): Unit = {
    val minus_ = minus.toSet
    underlying.formula = () => underlying.value.get.filter(!minus_.contains(_))
//    diffs.send((minus_, Set()))
    minus_.flatMap(_.getEventsources).foreach(_.underlying.freeze())
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

  override private[drx] def getEventsources: Set[EventSource[_]] =
    underlying.value.get.flatMap(_.getEventsources).toSet[EventSource[_]]
}

trait VarOwner extends VarLike {
  protected val children: mutable.Set[VarLike] = mutable.Set()

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

  private[drx] def getEventsources: Set[EventSource[_]] =
    children.toSet[VarLike].flatMap(_.getEventsources)
}
