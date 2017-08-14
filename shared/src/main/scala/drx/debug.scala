package drx

import scala.collection.mutable

/** Created by david on 10.06.17. */
object debug {

  val useOwnership = false

  /** only for ghost testing / debugging variables */
  class NoMap[K, V] {
    def update(a: K, b: V): Unit = Unit
    def map[R](x: (K, V) => R): List[R] = List.empty
    def keys: List[K] = List.empty
    def size = 0
  }

  private[drx] val debugSigs = compat2.compat2.PotentialWeakHashMap[Signal[_], Unit]()
  private[drx] val debugVars = compat2.compat2.PotentialWeakHashMap[Var[_], Unit]()
  private[drx] val debugObs  = compat2.compat2.PotentialWeakHashMap[Observer[_], Unit]()

  def printless(): Unit = {
    println(compat2.compat2.heapSize())
//    println("v=" + debugVars.size + " s=" + debugSigs.size +
//      " o=" + debugObs.size + " heap=" + compat2.heapSize() / 10000000)
  }

  def doit() = {
    printless()
    for (i <- 0 to 5) compat2.compat2.gc()
    printless()
  }

  def serialize(x: Any): String = x match {
    case None                      => "none"
    case Some(y)                   => serialize(y)
    case x: Rx[_]                  => x.id
    case x: Observer[_]            => x.id
    // case x: Node => x.outerHTML.replace('"', "'")
    case x: mutable.Map[_, _]      =>
      "{" + x.map {
        case (a, b) => "" + serialize(a) + ":" + serialize(b)
      }.mkString(", ") + "}"
    case x: mutable.Traversable[_] =>
      "[" + x.map(it => serialize(it)).mkString(", ") + "]"
    case _ => "" + x
  }

  def getthem(rx: Rx[_], acc: mutable.Set[Rx[_]]): Unit = if (!acc.contains(rx)) rx match {
    case x: Var[_]    => acc += x
      x.out.foreach { y => getthem(y, acc) }
    case x: Signal[_] => acc += x
      x.in.foreach { y => getthem(y, acc) }
      x.out.foreach { y => getthem(y, acc) }
  }

  def transitivehull(xs: Set[Rx[_]]): Set[Rx[_]] = {
    val set = mutable.Set[Rx[_]]()
    xs.foreach(getthem(_, set))
    set.toSet
  }

  def transitivehullobservers(xs: Set[Observer[_]]): Set[Observer[_]] = {
    val set = mutable.Set[Rx[_]]()
    xs.map(_.observed).foreach(getthem(_, set))
    set.toSet[Rx[_]].map(_.observers).flatten
  }

  def linked[X](a: Observer[X]): Boolean = a.observed.observers.contains(a)

  def stringit(root: Set[Observer[_]] = Set()): String = {

    val obs = root ++ debug.debugObs.keys
    val rxs = transitivehull(obs.map(_.observed))
    val vars: Set[Var[_]] = rxs.collect { case x: Var[_] => x } ++ debug.debugVars.keys
    val sigs: Set[Signal[_]] =  rxs.collect { case x: Signal[_] => x } ++ debug.debugSigs.keys

    ("digraph {\n" +

      vars.map { it => (

        s"""  "${it.id}" [style=filled,fillcolor="#00aadd",tooltip="""" +

          serialize(it.value) + "\"]\n" + it.out.map {
            child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
          }.mkString("\n")

        )}.mkString("\n") + "\n\n" +

      sigs.map {it => (

        s"""  "${it.id}" [style=filled,fillcolor="${if(it.calcActive) "#aadd00" else "silver"}",tooltip="${serialize(it.value)}"]\n""" +

          it.in.filter {
            dep => !dep.out.contains(it)
          }.map {
            dep => s"""  "${dep.id }" -> "${it.id }" [color=silver dir=back]"""
          }.mkString("\n") +

          it.out.map {
            child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
          }.mkString("\n") +

          it.observers.map {
            obs => s"""  "${it.id }" -> "${obs.id }" [dir=both]"""
          }.mkString("\n") +

//          it.createdObservers.map {
//            child => s"""  "${it.id }" -> "${child.id }" [color=orange]"""
//          }.mkString("\n") +

          "\n"

        )}.mkString("\n") +

      obs.map { it => (

        s"""  "${it.id}" [style=filled,fillcolor="${if(it.isActive) "#ddaa00" else "silver"}",tooltip="${serialize("elem") }"]""" +

          (if (linked(it)) ""
          else s"""  "${it.observed.id }" -> "${it.id}" [color=silver,dir=back]""")

        )}.mkString("\n") + "\n\n" +

      "\n}")
  }
}
