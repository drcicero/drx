package drx

import scala.collection.mutable
import scala.util.Success

/** create using [[VarOwner.mkStore]] */
sealed class Store[X <: VarLike, CtorArgs](ctor: CtorArgs => X, name: String = "")
  extends EventSource[List[X]](SignalKind, name) {

  underlying.value = Success(List[X]())
//  val diffs = new Channel[(List[X], List[X])]("diff")

  def create(args: CtorArgs, name: String = ""): X = {
    var vari: Option[X] = None
    withTransaction { tx =>
      vari = Some(ctor(args))
      underlying.formula = () => underlying.value.get ++ Seq(vari.get)
//      diffs.send((List(), List(vari.get)))
      tx.markSource(underlying)
    }
    vari.get
  }

  def remove(minus: List[X]): Unit = {
    underlying.formula = () => underlying.value.get.filter(!minus.contains(_))
//    diffs.send((minus, List()))
    minus.flatMap(_.getEventsources).foreach(_.underlying.freeze())
    withTransaction(_.markSource(underlying))
  }

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
