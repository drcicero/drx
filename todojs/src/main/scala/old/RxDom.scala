package old

import drx.concreteplatform
import drx.graph.{Obs, Rx}
import org.scalajs.dom
import org.scalajs.dom.Element
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic.{AttrPair, StylePair}

import scala.collection.mutable
import scala.scalajs.js

/** Created by david on 15.09.17. */
object RxDom {

  // for some subtypes of Modifier, implicit conversions aproximating
  //   Rx[Modifier] ==> Modifier
  implicit def tagToMod[X <: Element](sig: Rx[TypedTag[X]]): Modifier = (parent: Element) => {
    var oldelem: Element = span("{{init}}").render
    val sinkTag = sig.map(_.render).mkForeach { newelem =>
      replaceChild(parent, oldelem, newelem)
      oldelem = newelem
    }
    addSink(oldelem, sinkTag)
    parent.appendChild(oldelem)
  }

  implicit def styleToMod(sig: Rx[StylePair[Element, _]]): Modifier = (parent: Element) => {
    val sinkStyle = sig.mkForeach { mod => mod.applyTo(parent) }
    addSink(parent, sinkStyle)
  }

  implicit def attrToMod(sig: Rx[AttrPair[Element, _]]): Modifier = (parent: dom.Element) => {
    val sinkAttr = sig.mkForeach { mod =>
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

  implicit class RxDMapMap[X](val diffs: Rx[TraversableOnce[(String, Option[X])]]) extends AnyVal {
    def dmapmap(fun: X => TypedTag[Element]): Modifier = (parent: Element) => {
      val map = mutable.Map[String, Element]()
      val sinkDiff = diffs.map { diffmap =>
        diffmap
      }.mkForeach { diffmap =>
//        val diffmap = diffmapy.map { case (k, optmk) =>
//          k -> optmk.map(mk => fun(mk).render) }
        diffmap foreach { case (k, optnewelem) =>
          val z = map remove k
          optnewelem foreach { newelemy =>
            val newelem = fun(newelemy).render
            map += (k -> newelem)
            insertChild(parent, newelem)
          }
          z foreach (removeChild(parent, _))
        }
      }
      addSink(parent, sinkDiff)
    }
  }

  def insertChild(parent: dom.Element, elem: dom.Element): Unit = {
    elem.classList.add("rx-building")
    parent.appendChild(elem)
//    if (isRooted(elem))
      foreachSink(elem)(_.start()) // TODO originally switch with next line
    //    elem.classList.remove("rx-building")
  }
  def replaceChild(parent: dom.Element, oldelem: dom.Element, newelem: dom.Element): Unit = {
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }
    newelem.classList.add("rx-building")
//    if (isRooted(parent)) {
      foreachChildSink(oldelem)(_.stop()) // stop unrooted sinks
      foreachSink(newelem)(_.start())     // start rooted sinks
//    }
    parent.appendChild(newelem) // root newelem // TODO or before isRooted?
    parent.removeChild(oldelem) // root newelem // TODO or before isRooted?
//    elem.classList.remove("rx-building")
  }
  def removeChild(parent: dom.Element, elem: dom.Element): Unit = {
    parent.removeChild(elem)
//    if (isRooted(elem))
      foreachSink(elem)(_.stop()) // TODO or one line below?
  }

//  def attrValue(sig: Rx[Modifier]): Modifier =
//    (t: dom.Element) => sig.mkObs { mod =>
//      mod.applyTo(t)
//    }

//  implicit def styleValue[T: StyleValue](t: dom.Element, a: Style, signal: Signal[T]): Unit =
//      signal.foreach { value => implicitly[StyleValue[T]].apply(t, a, value) }

//  def replaceLastChild(parent: dom.Element, newelem: dom.Element): Unit = {
//    val visible = isRooted(parent)
//    if (visible) foreachSink(parent)(_.stop())
//    parent.replaceChild(newelem, parent.lastChild)
//    if (visible) foreachSink(parent)(_.start())
//  }

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }

  private val DATA_IS_REACTIVE = "data-is-reactive"
  private val sinkMap = concreteplatform.WeakMap[dom.Node, mutable.Set[Obs[_]]]()

  private def addSink(it: dom.Element, obs: Obs[_]): Unit = {
    val sinks = sinkMap.get(it).getOrElse {
      val tmp = mutable.Set[Obs[_]]()
      sinkMap.set(it, tmp)
      it.setAttribute(DATA_IS_REACTIVE, "true")
      tmp
    }
    sinks += obs
//    dom.console.log(sinkMap)
  }

  private def remSink(it: dom.Element, obs: Obs[_]): Unit = {
    val sinks = sinkMap.get(it).get
    sinks -= obs
    if (sinks.isEmpty) it.removeAttribute(DATA_IS_REACTIVE)
//    dom.console.log(sinkMap)
  }

  def collectChildSinks(fc: dom.Element): Set[Obs[_]] = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length;
          y <- sinkMap.get(list(i)).getOrElse(mutable.Set()))
      yield y).toSet[Obs[_]]
  }
//  platform.platform.collector = () => collectChildSinks(dom.document.body)

  private def foreachChildSink(fc: dom.Element)(f: Obs[_] => Unit): Unit =
    collectChildSinks(fc).foreach(f)
  private def foreachSink(fc: dom.Element)(f: Obs[_] => Unit): Unit =
    (collectChildSinks(fc) ++ sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)

  private def isRooted(node: dom.Node): Boolean =
    dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]

}
