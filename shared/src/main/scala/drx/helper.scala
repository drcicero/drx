package drx

import scala.collection.mutable

/** Created by david on 10.06.17. */
object helper {

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

  def linked[X](a: Observer[X]): Boolean = a.observed.observers.contains(a)

  def stringit(root: Set[Observer[_]] = Set()): String = {

    val obs = root ++ debugObs.keys
    val rxs = transitivehull(obs.map(_.observed))
    val vars: Set[Var[_]] = rxs.collect { case x: Var[_] => x } ++ debugVars.keys
    val sigs: Set[Signal[_]] =  rxs.collect { case x: Signal[_] => x } ++ debugSigs.keys

    ("digraph {\n" +

      vars.map { it => (

        s"""  "${it.id}" [style=filled,fillcolor="${if(it.isActive) "#00aadd" else "silver"}",tooltip="""" +

          serialize(it.value) + "\"]\n" + it.out.map {
            child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
          }.mkString("\n")

        )}.mkString("\n") + "\n\n" +

      sigs.map {it => (

        s"""  "${it.id}" [style=filled,fillcolor="${if(it.isActive) "#aadd00" else "silver"}",tooltip="${serialize(it.value)}"]\n""" +

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

          it.createdObservers.map {
            child => s"""  "${it.id }" -> "${child.id }" [color=orange]"""
          }.mkString("\n") + "\n"

        )}.mkString("\n") +

      obs.map { it => (

        s"""  "${it.id}" [style=filled,fillcolor="${if(it.isActive) "#ddaa00" else "silver"}",tooltip="${serialize("elem") }"]""" +

          (if (linked(it)) ""
          else s"""  "${it.observed.id }" -> "${it.id}" [color=silver,dir=back]""")

        )}.mkString("\n") + "\n\n" +

      "\n}")
  }

}
