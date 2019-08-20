package drx.interface

import java.util.concurrent.ThreadLocalRandom

import VarMap.mapmapOps
import drx.concreteplatform
import drx.interface.DSL.{Owner, BagVar, Val, Var}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

class BagBuilder[X](private val underlying: mutable.Map[X, Int] = mutable.Map[X, Int]()) extends mutable.Builder[(X, Int), Bag[X]] {
  //println("before " + underlying.toIndexedSeq)
  override def +=(elem: (X, Int)): this.type = {
    val next = underlying.getOrElse(elem._1, 0) + elem._2
    //println("  += " + (elem._1, underlying.getOrElse(elem._1, 0), elem._2, next))
    if (next != 0) underlying(elem._1) = next
    else underlying.remove(elem._1)
    this }
  override def clear(): Unit = underlying.clear()
  override def result(): Bag[X] = {
    //println("now    " + underlying.toIndexedSeq + "\n")
    new Bag(underlying.toMap)
  }
}
class Bag[X](private val underlying: Map[X, Int] = Map[X, Int]()) {
  def size: Int = underlying.values.sum
  def isEmpty: Boolean = underlying.isEmpty
  def nonEmpty: Boolean = underlying.nonEmpty

  def apply(key: X): Int = underlying.getOrElse(key, 0)
  def merge(that: Bag[X], debug: Boolean = false)(op: (Int, Int) => Int): Bag[X] = {
    val keys = this.underlying.keys ++ that.underlying.keys
    VarMap.idebug("merge ", keys.map[(X, Int), Bag[X]] { k =>
      VarMap.idebug("%s = %s + %s = m(%s)".format(op(this (k), that(k)), this (k), that(k), k),
        k -> op(this(k), that(k)))
    }(Bag.canBuildBagFromIterable))
  }

  def mapValues(f: Int => Int): Bag[X] =
    new Bag(underlying.mapValues(f))
  def mapI[Y](f: ((X, Int)) => Y): Bag[Y] =
    flatMapI[Y](x => new Bag[Y](Map[Y, Int](f(x) -> 1)))
  def map[Y](f: X => Y): Bag[Y] =
    flatMap[Y](x => new Bag[Y](Map[Y, Int](f(x) -> 1)))
  def flatMapI[Y](f: ((X, Int)) => Bag[Y]): Bag[Y] = {
    val bagBuilder = new BagBuilder[Y]()
    underlying.foreach { case (k, v) => f(k, v).foreachI { case (k2, v2) =>
      bagBuilder += k2 -> v
    } }
    bagBuilder.result()
  }
  def flatMap[Y](f: X => Bag[Y]): Bag[Y] = {
    val bagBuilder = new BagBuilder[Y]()
    underlying.foreach { case (k, v) => f(k).foreachI { case (k2, v2) =>
      bagBuilder += k2 -> (v * v2)
    } }
    bagBuilder.result()
  }
  def fold[Y](init: Y)(f: (Y, X) => Y): Y = {
    var state = init
    for (elem <- this.underlying) { for (_ <- (0 until elem._2)) {
        state = f(state, elem._1)
    } }
    state
  }

  def foreachI[Y](f: ((X, Int)) => Y): Unit = underlying.foreach(f)
  def foreach[Y](f: X => Y): Unit = underlying.foreach(x => f(x._1))
  def iteratorI: Iterator[(X, Int)] = underlying.iterator
  def iterator: Iterator[X] = underlying.iterator.flatMap { case (k,v) => (0 until v).map(_ => k) }
}
object Bag {
  def canBuildBagFromIterable[X]: CanBuildFrom[Iterable[X], (X, Int), Bag[X]] = new CanBuildFrom[Iterable[X], (X, Int), Bag[X]] {
    override def apply(): mutable.Builder[(X, Int), Bag[X]] = new BagBuilder[X]
    override def apply(from: Iterable[X]): mutable.Builder[(X, Int), Bag[X]] = apply()
  }
  def canBuildBagFromMap[X]: CanBuildFrom[Iterable[(X, Int)], (X, Int), Bag[X]] = new CanBuildFrom[Iterable[(X, Int)], (X, Int), Bag[X]] {
    override def apply(): mutable.Builder[(X, Int), Bag[X]] = new BagBuilder[X]
    override def apply(from: Iterable[(X, Int)]): mutable.Builder[(X, Int), Bag[X]] = apply()
  }
  implicit def traversableBag[X](bag: Bag[X]): Traversable[X] = new Traversable[X] {
    override def foreach[U](f: X => U): Unit = bag.foreach(f) }
  implicit def traversableBagI[X](bag: Bag[X]): Traversable[(X, Int)] = new Traversable[(X, Int)] {
    override def foreach[U](f: ((X, Int)) => U): Unit = bag.foreachI(f) }
}

final class VarMap[X <: Owner] extends Owner {
  val diffs: BagVar[X] = BagVar[X]()
  val aggregate: Val[Bag[X]] = diffs.mkAggregate(this)
  def sampleAggregate: Bag[X] = aggregate.sample
  def update(deltas: Bag[X]): Unit = diffs.update(deltas)
  def filterInPlace(func: X => Boolean): Unit = update(sampleAggregate.flatMap(x =>
    new Bag(Map(x -> (if (func(x)) 0 else -1))) ))
}

object VarMap {
  var i = 2
  var empty = true
  def idebug[X](msg: String, x: => X): X = {
    if (msg != "") println("  ".repeat(i) + msg)
    i+=1
    empty = true
    val result = x
    i-=1
    if (!empty) println("  ".repeat(i) + "end")
    empty = false
    result
  }

  def apply[X <: Owner]() = new VarMap[X]()

  implicit class flattenOps[X](val rx: Val[Bag[Val[Bag[X]]]]) extends AnyVal {
    def flatten(owner: Owner): Val[Bag[X]] =
      rx.map { ev => idebug("flatten", {
        val bagBuilder = new BagBuilder[X]()
        ev.foreachI { case (k, v) => k.get.foreachI { case (k2, v2) =>
          bagBuilder += k2 -> (v * v2)
        } }
        bagBuilder.result()
      }) }
  }
  implicit class mapmapOps[X](val rx: Val[Bag[X]]) extends AnyVal {
    def mapFilter(fun: X => Boolean)(owner: Owner): Val[Bag[X]] =
      rx.map { bag => bag map { v: X => Val(new Bag(Map(v -> (if (fun(v)) 1 else 0)))) } }.flatten(owner)
    def mapMap[Y](fun: X => Y)(owner: Owner): Val[Bag[Y]] =
      rx.map { bag => bag map { v: X => Val(new Bag(Map(fun(v) -> 1))) } }.flatten(owner)
    def mapFlatMap[Y](fun: X => Bag[Y])(owner: Owner): Val[Bag[Y]] =
      rx.map { bag => bag map { v: X => Val(fun(v)) } }.flatten(owner)
    def mkAggregate(owner: Owner, debug: Boolean = false): Val[Bag[X]] =
      rx.scan(new Bag[X]())( (state,ev) => state.merge(ev, debug)(_+_))(owner)
  }

  def hexRnd(): String = ThreadLocalRandom.current().nextLong().toHexString

}

class Table[This <: Owner : reflect.ClassTag] extends Owner { self: This =>
  val id: String = VarMap.hexRnd()
  Table.getRaw[This].update(new Bag(Map(this -> 1)))
  def Vary[X](init: X): Var[X] = drx.interface.DSL.Var(init)
  def delete(): Unit = Table.getRaw[This].filterInPlace(_ != this)
}

object Table extends Owner {
  private val tables: mutable.Map[Class[_], VarMap[_ <: Owner]] = mutable.Map()
  private[interface] def getRaw[C <: Owner : reflect.ClassTag]: VarMap[C] =
    tables.getOrElseUpdate(implicitly[reflect.ClassTag[C]].runtimeClass, { VarMap[C]() }).asInstanceOf[VarMap[C]]
  def getData[C <: Owner : reflect.ClassTag]: Val[Bag[C]]  = getRaw[C].aggregate
  def getDelta[C <: Owner : reflect.ClassTag]: Val[Bag[C]] = getRaw[C].diffs

  def extend[X <: Owner : reflect.ClassTag, Y](func: X => Y): X => Y = {
    val cache = concreteplatform.WeakMap[X, Y]()
    val accessor = (x: X) => cache.get(x).getOrElse {
      val tmp = func(x)
      cache.set(x, tmp)
      tmp
    }
    Table.getRaw[X].aggregate.sample.foreach(accessor)
    Table.getRaw[X].diffs.mapMap(accessor)(this).enable()
    accessor
  }
}
