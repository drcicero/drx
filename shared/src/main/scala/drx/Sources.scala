package drx

import scala.collection.mutable

trait EventSource[X] extends GraphNode[X] with Signal[X]

private[drx] trait VarLike {
  private[drx] def getEventsources: Set[EventSource[_]]
}

/** create [[Variable]] and [[Source]] through [[VarOwner]]s. for example
  * ~~~
  * object state extends VarOwner { val x = mkVar(0) }
  * ~~~ */
trait VarOwner extends VarLike {
  protected val children: mutable.Set[VarLike] = mutable.Set()
  def mkSource[X](name: String = ""): Source[X] = {
    val result = new Source[X](name)
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

/** create using [[VarOwner.mkSource]] */
sealed class Source[X] private[drx](name: String = "") extends GraphNode[X](name) with EventSource[X] with VarLike {
  def fire(newValue: X): Unit = { value = Some(newValue); withContext { tx => tx.markSource(this); tx.markSink { () => value = None } } }
  private[drx] def getEventsources: Set[EventSource[_]] = Set(this)
}

/** create using [[VarOwner.mkVar]] */
sealed class Variable[X] private[drx](init: X, name: String = "") extends GraphNode[X](name) with EventSource[X] with VarLike {
  value = Some(init)
  def set(newValue: X): Unit = { value = Some(newValue); withContext(_.markSource(this)) }
  def transform(transformer: X => X): Unit = set(transformer(value.get))
  private[drx] def getEventsources: Set[EventSource[_]] = Set(this)
}

/** create using [[VarOwner.mkStore]] */
sealed class Store[X <: VarLike, CtorArgs] private[drx](ctor: CtorArgs => X, name: String = "")
  extends GraphNode[Set[X]](name) with EventSource[Set[X]] with VarLike {
  value = Some(Set[X]())
  def create(args: CtorArgs, name: String = ""): X = withContext { tx =>
    val vari = ctor(args)
    value = Some(value.get + vari)
    tx.markSource(this)
    vari
  }
  def remove(vari: X): Unit = {
    value = Some(value.get - vari)
    vari.getEventsources.foreach(_.kill())
    withContext(_.markSource(this))
  }
  private[drx] def getEventsources: Set[EventSource[_]] = value.get.flatMap(_.getEventsources)
}
