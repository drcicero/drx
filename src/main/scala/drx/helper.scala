package drx

import scala.collection.mutable

import org.scalajs.dom
import org.scalajs.dom.raw.Node
import org.scalajs.dom.{Element, document}

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

  def printless(): Unit = {
    println("v=" + debugVars.size + " s=" + debugSigs.size +
      " o=" + debugObs.size)
  }

  def stringit(): String = (

    "digraph {\n" + debugVars.map { case (it, _) => (

      s"""  "${it.id}" [style=filled,fillcolor="#00aadd",tooltip="""" +

        serialize(it.value) + "\"]\n" + it.out.map {
        child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
      }.mkString("\n")

      )}.mkString("\n") + "\n\n" +

      debugSigs.map {case (it, _) => (

        s"""  "${it.id}" [style=filled,fillcolor="#aadd00",tooltip="${serialize(it.value)}"] """ +

          it.in.filter {
            dep => !dep.out.contains(it)
          }.map {
            dep => s"""  "${dep.id }" -> "${it.id }" [color=gray dir=back]"""
          }.mkString("\n") + "\n" +

          it.out.map {
            child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
          }.mkString("\n") + "\n" +

          it.observers.map {
            obs => s"""  "${it.id }" -> "${obs.id }""""
          }.mkString("\n") + "\n" +

          it.createdObservers.map {
            child => s"""  "${it.id }" -> "${child.id }" [color=orange]"""
          }.mkString("\n") + "\n"

        )}.mkString("\n") +

      debugObs.map {case (_, it) => (

        s"""  "${it.id}" [style=filled,fillcolor="#ddaa00",tooltip="${serialize("elem") }"]""" +

          s"""  "${it.observed.id }" -> "${it.id }" [color=gray,dir=back]"""

        )}.mkString("\n") + "\n\n" +

      "\n}")

}
