/** this package implements Rx, Var, Obs */
package drx

import scala.collection.mutable
import scala.util.{DynamicVariable, Failure, Success, Try}

object grouped {
  def apply[X](changer: => Unit): Unit = withContext { _ => changer }
}


sealed abstract class GraphNode[X] private[drx](name: String) {
  debug.debugRxs(this) = Unit

  val id: String = "" + internals.count + name

  private[drx] val outs: mutable.Set[DerivedValue[_]] = mutable.Set()
  private[drx] val ins: mutable.Set[GraphNode[_]] = mutable.Set()
  private[drx] var level: Int = 0
  private[drx] def isNeeded: Boolean = outs.nonEmpty
  private[drx] def getOuts: Set[DerivedValue[_]] = outs.toSet
  private[drx] def unsubscribe(sig: DerivedValue[_]): Unit = {
    if (!outs.contains(sig)) throw new RuntimeException("redundant unsubscribe from " + this.id + " to "+ sig.id)
    else println("  unsubscribe from " +this.id+" to "+sig.id)
    outs -= sig
  }
  private[drx] var frozen: Boolean = false
  private[drx] def freeze(): Unit = {
    frozen = true
    outs.foreach { it => if (it.ins.forall(_.frozen)) it.freeze() }
  }

  private def maxOrZero(lst: TraversableOnce[Int]) = try lst.max catch { case _: UnsupportedOperationException => 0 }
  private[drx] def linkWith(to: DerivedValue[_])(ifneedsreevaluation: => Unit): Unit = {
    if (!this.outs.contains(to)) println("  subscribe from "+this.id +" to "+to.id)

    val wasActive = this.isNeeded
    this.outs += to

    to.ins += this
    val newLevel = maxOrZero(to.ins.map(_.level)) + 1
    val levelup = newLevel > to.level
    if (levelup) to.levelup(newLevel)

    if (levelup || !wasActive || value.isEmpty) ifneedsreevaluation
  }
  private[drx] def subscribe(to: DerivedValue[_]): Unit
  private[drx] var value: Option[X] = None

  // private[drx] val dummy = Array.tabulate(1000 * 100)( i => i )
}

sealed trait Signal[X] extends GraphNode[X] {
  def now: X = value.get
  def get: X = { subscribe(internals.activeSig.value.get); now }
  def map[Y](func: X => Y, name: String = ""): DynamicSignal[Y] = Signal( func(get), name )
  def fold[Y](init: Y, owner: VarOwnerLike = internals.dummyOwner)(comb: (Y, X) => Y): DynamicSignal[Y] = {
    val that = this
    var tmp: Y = init
    val result = new DynamicSignal[Y]({ () => tmp = comb(tmp, get); tmp }, "fold") with Startable[Y] { override def parent: Signal[X] = that }
    result.start()
    checksafety(owner)
    result
  }
  /** you can register a callback via [[foreach]], that starts immediately. */
  def mkObs(callback: X => Unit): Callback[X] = new Callback(this, callback)
  /** you can create a callback via [[mkObs]], that is deactive at the beginning and can be started later. */
  def foreach(callback: X => Unit): Callback[X] = {
    val tmp = new Callback(this, callback)
    tmp.start()
    tmp
  }

  private def checksafety(owner: VarOwnerLike) = {
    val ancestry: mutable.Set[GraphNode[_]] = mutable.Set()
    def transitiveIns(it: GraphNode[_]): Unit = {
      if (ancestry.add(it)) it.ins.foreach(transitiveIns)
    }
    transitiveIns(this)
    val vars = ancestry.collect { case it: Variable[_] => it }
    if (vars.size > 1 && !vars.subsetOf(owner.getVariables.toSet))
      throw new RuntimeException("You are creating a fold over multiple variables." +
        " The fold will live until all incoming variables are dead.." +
        " You must specify the VarOwner of all Variables that influence this fold." +
        " for example like: a = owner.mkVar(); b = owner.mkVar() _.fold(0, owner)(_+_) ." +
        " The fold will be then be collected when this VarOwner is collected.")
  }
}

sealed trait DerivedValue[X] extends GraphNode[X] {
  private[drx] def levelup(newLevel: Int): Unit = {
    level = newLevel
    outs.foreach(_.levelup(newLevel + 1))
  }
  private[drx] override def freeze() = formula = () => value.get
  override private[drx] def unsubscribe(sig: DerivedValue[_]) = { super.unsubscribe(sig); checkStillActive() }
  override private[drx] def subscribe(to: DerivedValue[_]): Unit =
    linkWith(to) { withContext(_.markSig(this)); throw RetryLater }
  private[drx] def checkStillActive() = if (!isNeeded) ins.foreach(_.unsubscribe(this))
  private[drx] var formula: () => X
  private[drx] def reeval(): Unit = {
    if (!isNeeded) return // throw new RuntimeException("should not reeval inactive signal " + id)
    val tmpIn = Set() ++ ins; ins.clear()
    val result = Try(internals.activeSig.withValue(Some(this)) { formula() })
    result match {
      case Success(v) =>
        (tmpIn -- ins).foreach(_.unsubscribe(this))
        if (v != value) withContext(tx => outs.foreach(tx.markSig))
        value = Some(v)
      case Failure(exc) =>
        (tmpIn -- ins).foreach(_.unsubscribe(this)) // TODO this might be important
        if (exc != RetryLater) throw exc
    }
  }
}

trait EventSource[X] extends Signal[X]

trait VarOwnerLike {
  private[drx] def getVariables: Set[Variable[_]]
}

trait VarOwner extends VarOwnerLike {
  protected val children: mutable.Set[VarOwnerLike] = mutable.Set()
  def mkVar[X](init: X, name: String = ""): Variable[X] = {
    val result = new Variable(init, name)
    children += result
    result
  }
  def mkStore[X <: VarOwnerLike, CtorArgs](ctor : CtorArgs => X, name: String = ""): Store[X, CtorArgs] = {
    val result = new Store(ctor, name)
    children += result
    result
  }
  private[drx] def getVariables: Set[Variable[_]] = children.toSet[VarOwnerLike].flatMap(_.getVariables)
}

/** create using [[VarOwner.mkStore]] */
sealed class Store[X <: VarOwnerLike, CtorArgs] private[drx](ctor : CtorArgs => X, name: String = "")
  extends GraphNode[Set[X]](name) with EventSource[Set[X]] with VarOwnerLike {
  value = Some(Set[X]())
  override private[drx] def subscribe(to: DerivedValue[_]): Unit = linkWith(to) {}
  def create(args: CtorArgs, name: String = ""): X = {
    val vari = ctor(args)
    value = Some(value.get + vari)
    withContext(_.markVar(this))
    vari
  }
  def kill(vari: X): Unit = {
    value = Some(value.get - vari)
    vari.getVariables.foreach(_.freeze())
    withContext(_.markVar(this))
  }
  private[drx] def getVariables: Set[Variable[_]] = value.get.toSet[VarOwnerLike].flatMap(_.getVariables)
}

/** create using [[VarOwner.mkVar]] */
sealed class Variable[X] private[drx] (init: X, name: String = "") extends GraphNode[X](name) with EventSource[X] with VarOwnerLike {
  if (internals.activeSig.value.isDefined) throw new RuntimeException(
    "Creating variables inside signals is likely to introduce memory leaks.")

  value = Some(init)
  def set(newValue: X): Unit = { println(s"set $id $now to $newValue"); value = Some(newValue); withContext(_.markVar(this)) }
  def transform(transformer: X => X): Unit = set(transformer(now))
  override private[drx] def subscribe(to: DerivedValue[_]): Unit = linkWith(to) {}
  private[drx] def getVariables: Set[Variable[_]] = Set(this)
}

/** create using [[Signal.apply]] */
sealed class DynamicSignal[X] private[drx](private[drx] var formula: () => X, name: String) extends GraphNode[X](name) with DerivedValue[X] with Signal[X]
object Signal { def apply[X](formula: => X, name: String = ""): DynamicSignal[X] = new DynamicSignal(formula _, name) }

/** create using [[Signal.mkObs]] */
sealed class Callback[X] private[drx](private[drx] val parent: Signal[X], callback: X => Unit) extends GraphNode[Unit]("obs") with DerivedValue[Unit] with Startable[Unit] {
  private[drx] var formula = () => { val tmp = parent.get; withContext(_.markObs{() => callback(tmp)}) }
  level = Int.MaxValue // run as late as possible
}

private[drx] sealed trait Startable[X] { self: DerivedValue[X] =>
  var active = false
  override private[drx] def isNeeded = active
  def start(): Unit = if (!active) {
    if (internals.activeSig.value.isDefined) throw new RuntimeException(
      "You are starting an observer / creating a fold inside a signal. That very well may lead to memory leaks.")
    active = true
    try parent.subscribe(this) catch { case RetryLater => }
  }
  def stop(): Unit = if (active) { active = false; parent.unsubscribe(this) }
  private[drx] def parent: Signal[_]
}

private object internals {
  private var uniqueCtr = 0
  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val activeSig: DynamicVariable[Option[DerivedValue[_]]] = new DynamicVariable(None)
  val dummyOwner = new VarOwner {}
}