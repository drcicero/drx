package drx.interface

import java.util.concurrent.ThreadLocalRandom

import drx.concreteplatform
import drx.interface.DSL.{Owner, SeqVar, Val, Var}

import scala.collection.mutable

sealed abstract class Polarized[X]
case class Pos[X](content: X) extends Polarized[X]
case class Neg[X](content: X) extends Polarized[X]

final class VarMap[X <: Owner] extends Owner {
  val diffs: SeqVar[(String, Polarized[X])] = SeqVar[(String, Polarized[X])]()
  val aggregate: Val[Map[String, X]] = diffs.scan(Map[String, X]())(VarMap.add[X])(this)
  def sampleAsDelta: Seq[(String, Polarized[X])] = aggregate.sample.mapValues(x => Pos(x)).toSeq
  def update(delta: Seq[(String, Polarized[X])]): Unit = delta.foreach(diffs.set)
  def remove(delta: Seq[String]): Unit = { val tmp = aggregate.sample; update(delta.map(k => (k, Neg(tmp(k))))) }
  def keep(func: X => Boolean): Unit = remove(aggregate.sample.filter(x => func(x._2)).keys.toSeq)
}

object VarMap {
  def apply[X <: Owner]() = new VarMap[X]()
  def add[X <: Owner](x: Map[String, X], y: Seq[(String, Polarized[X])]): Map[String, X] = {
    // y collect { case (k, Neg(v)) => v.children.foreach(_.disable()) }
    (x.mapValues(x => Pos(x)) ++ y) collect
      { case (k, Pos(v)) => (k, v) }
  }

  implicit class ImapmapValues[X,Y](val rx: Val[TraversableOnce[(String, Polarized[X])]]) extends AnyVal {
    def mapmapValues(fun: X => Y): Val[TraversableOnce[(String, Polarized[Y])]] =
      rx.map { seq => seq map {
        case (k, Pos(v)) => k -> Pos(fun(v))
        case (k, Neg(v)) => k -> Neg(fun(v))
      } }
  }

  def hexRnd(): String = ThreadLocalRandom.current().nextLong().toHexString

}

class Table[This <: Owner : reflect.ClassTag] extends Owner { self: This =>
  val id: String = VarMap.hexRnd()
  Table.getRaw[This].update(Seq((id, Pos(this))))
  def Vary[X](init: X): Var[X] = drx.interface.DSL.Var(init)
  def delete(): Unit = Table.getRaw[This].remove(Seq(id))
}

object Table {
  private val tables: mutable.Map[Class[_], VarMap[_ <: Owner]] = mutable.Map()
  private[interface] def getRaw[C <: Owner : reflect.ClassTag]: VarMap[C] =
    tables.getOrElseUpdate(implicitly[reflect.ClassTag[C]].runtimeClass, { VarMap[C]() }).asInstanceOf[VarMap[C]]
  def getBag[C <: Owner : reflect.ClassTag]: Val[Iterable[C]]                 = getRaw[C].aggregate.map(_.values)
  def getDiffs[C <: Owner : reflect.ClassTag]: SeqVar[(String, Polarized[C])] = getRaw[C].diffs

  def extend[X <: Owner : reflect.ClassTag, Y](func: X => Y): X => Y = {
    val cache = concreteplatform.WeakMap[X, Y]()
    val accessor = (x: X) => cache.get(x).getOrElse {
      val tmp = func(x)
      cache.set(x, tmp)
      tmp
    }
    Table.getRaw[X].aggregate.sample.values.foreach(accessor)
    VarMap.ImapmapValues(Table.getRaw[X].diffs).mapmapValues(accessor).enable()
    accessor
  }
}
