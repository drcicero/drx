package standard

import drx.concreteplatform

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic.{AttrPair, StylePair}

import scala.collection.mutable
import scala.language.implicitConversions

import org.scalajs.dom
import org.scalajs.dom.Element
import scala.scalajs.js

import drx.interface.DSL._

// TODO never .render followed by appendChild. Then the component was not activated...
//      maybe fixed now

// TODO maybe convert to Frags instead of Modifiers?

/** Created by david on 15.09.17. */
object SDom {

  // for some subtypes of Modifier, implicit conversions aproximating
  //   Rx[Modifier] ==> Modifier
  implicit def tagToMod[X <: Element](sig: Val[TypedTag[X]]): Modifier = (parent: Element) => {
    var oldelem: Element = span("{{init}}").render
    val sinkTag = sig.map(x => x.render).map { newelem =>
      replaceChild(parent, oldelem, newelem)
      oldelem = newelem
    }
    addSink(oldelem, sinkTag)
    parent.appendChild(oldelem)
  }

  implicit def styleToMod[Y](sig: Val[StylePair[Element, Y]]): Modifier = (parent: Element) => {
    val sinkStyle = sig.map { mod: StylePair[Element, Y] => mod.applyTo(parent) }
    addSink(parent, sinkStyle)
  }

  implicit def attrToMod[Y](sig: Val[AttrPair[Element, Y]]): Modifier = (parent: Element) => {
    val sinkAttr = sig.map { mod: AttrPair[Element, _] =>
      // TODO? replace hacky code with unmodifiers (unnapplyto) in scalatags? alt: use scala-dom-types?
      if (mod.a.name == "value")
        parent.asInstanceOf[js.Dynamic].value = mod.v.asInstanceOf[String]
      else if (mod.a.name == "checked")
        parent.asInstanceOf[js.Dynamic].checked = mod.v.asInstanceOf[Boolean]
      else if (mod.a.name == "disabled")
        parent.asInstanceOf[js.Dynamic].disabled = mod.v.asInstanceOf[Boolean]
      else mod.applyTo(parent)
    }
    addSink(parent, sinkAttr)
  }

  implicit def seqToMod[X <: dom.Element](diffs: Val[TraversableOnce[(String, TypedTag[X])]]
                                         ): Modifier = (parent: Element) => {
    val sinkDiff = diffs.map { diffmap =>

      val lst = parent.childNodes
      // TODO why is reverse necessary?
      (0 until lst.length).reverse foreach { i => lst(i) match {
        case element: Element => removeChild(parent, element)
        case _ =>
      } }

      diffmap foreach { case (k, tag) =>
        val newelem = tag.render
        insertChild(parent, newelem)
      }

    }
    addSink(parent, sinkDiff)
  }

  implicit def deltaToMod[X <: dom.Element](diffs: Val[TraversableOnce[(String, Polarized[TypedTag[X]])]]
                                         ): Modifier = (parent: Element) => {
    val map = mutable.Map[String, Element]()
    val sinkDiff = diffs.map { diffmap =>
      diffmap foreach { case (k, pol) =>
        val oldelem = map remove k
        pol match {
          case Pos(content) =>
            val newelem = content.render
            map(k) = newelem
            if (oldelem.isDefined)
              replaceChild(parent, oldelem.get, newelem)
            else
              insertChild(parent, newelem)
          case Neg(content) if (oldelem.isDefined) =>
            removeChild(parent, oldelem.get)
          case _ =>
        }
      }
    }
    addSink(parent, sinkDiff)
  }

  def insertChild(parent: dom.Element, elem: dom.Element): Unit = {
    val placeholder: Element = span("{{init}}").render
    parent.appendChild(placeholder)
    replaceChild(parent, placeholder, elem)
  }
  def removeChild(parent: dom.Element, elem: dom.Element): Unit = {
    val placeholder: Element = span("{{dele}}").render
    replaceChild(parent, elem, placeholder)
    parent.removeChild(placeholder)
  }

  private val DATA_IS_REACTIVE = "data-is-reactive"
  private val sinkMap = concreteplatform.WeakMap[dom.Node, mutable.Set[Val[_]]]()

  private def addSink(it: dom.Element, obs: Val[_]): Unit = {
    val sinks = sinkMap.get(it).getOrElse {
      val tmp = mutable.Set[Val[_]]()
      sinkMap.set(it, tmp)
      it.setAttribute(DATA_IS_REACTIVE, "true")
      tmp
    }
    sinks += obs
  }

  private def remSink(it: dom.Element, obs: Val[_]): Unit = {
    val sinks = sinkMap.get(it).get
    sinks -= obs
    if (sinks.isEmpty) it.removeAttribute(DATA_IS_REACTIVE)
  }

  def collectChildSinks(fc: dom.Element): Set[Val[_]] = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length;
          y <- sinkMap.get(list(i)).getOrElse(mutable.Set()))
      yield y).toSet[Val[_]]
  }
  //  platform.platform.collector = () => collectChildSinks(dom.document.body)

  private def foreachChildSink(fc: dom.Element)(f: Val[_] => Unit): Unit =
    collectChildSinks(fc).foreach(f)
  private def foreachSink(fc: dom.Element)(f: Val[_] => Unit): Unit =
    (collectChildSinks(fc) ++ sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)

  private def isRooted(node: dom.Node): Boolean =
    dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]

  def replace(oldelem: dom.Element, newelem: dom.Element): Unit =
    replaceChild(oldelem.parentNode, oldelem, newelem)
  def replaceChild(parent: dom.Node, oldelem: dom.Element, newelem: dom.Element): Unit = {
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }

    val rooted = isRooted(parent)
    if (rooted)
      foreachChildSink(oldelem)(_.disable()) // stop unrooted sinks
    parent.replaceChild(newelem, oldelem) // note order of args: replaceChild(toInsert, toRemove)
    if (rooted)
      foreachSink(newelem)(_.enable())     // start rooted sinks
    newelem.classList.remove("rx-building")

    // why order stop, replace, start?
  }

}
