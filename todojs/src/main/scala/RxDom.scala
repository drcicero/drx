import drx.{Rx, Obs}
import org.scalajs.dom
import org.scalajs.dom.Element
import platform.platform.WeakMap
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic.{AttrPair, StylePair}

import scala.collection.{GenTraversableOnce, mutable}
import scala.language.implicitConversions
import scala.scalajs.js

// TODO never .render followed by appendChild. Then the component was not activated...
//      maybe fixed now

/** Created by david on 15.09.17. */
object RxDom {
  // for some subtypes of Modifier, implicit conversions aproximating
  //   Rx[Modifier] ==> Modifier
  implicit def tagToMod(sig: Rx[_ <: TypedTag[dom.Element]]): Modifier
    = RxElemModifier(sig).toMod
  implicit def styleToMod(sig: Rx[_ <: StylePair[dom.Element, _]]): Modifier
    = RxStyleModifier(sig).toMod
  implicit def attrToMod(sig: Rx[_ <: AttrPair[dom.Element, _]]): Modifier
    = RxAttrModifier(sig).toMod

  def insertChild(parent: dom.Element, elem: dom.Element): Unit = {
    parent.appendChild(elem)
    if (isRooted(parent)) foreachSink(elem)(_.start())
  }

  def replaceChild(parent: dom.Element, oldelem: dom.Element, newelem: dom.Element): Unit = {
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }
    parent.replaceChild(newelem, oldelem) // root newelem
    if (isRooted(parent)) {
      foreachChildSink(oldelem)(_.stop()) // stop unrooted sinks
      foreachSink(newelem)(_.start())     // start rooted sinks
    }
  }

  def removeChild(parent: dom.Element, elem: dom.Element): Unit = {
    parent.removeChild(elem)
    if (isRooted(parent)) foreachSink(elem)(_.stop())
  }


  implicit class RxElemModifier(val sig: Rx[_ <: TypedTag[dom.Element]]) extends AnyVal {
    def toMod: Modifier = (parent: Element) => {
      var oldelem: dom.Element = span("{{init}}").render
      val sink = sig.mkObs { newmk =>
        val newelem = newmk.render
        replaceChild(parent, oldelem, newelem)
        oldelem = newelem
      }
      addSink(oldelem, sink)
      parent.appendChild(oldelem)
    }
  }

  // TODO? implement unmodifiers (unnapplyto) in scalatags?
  implicit class RxAttrModifier(val sig: Rx[_ <: AttrPair[dom.Element, _]]) extends AnyVal {
    def toMod: Modifier = (parent: dom.Element) => {
      val sink = sig.mkObs { mod =>
        if (mod.a.name == "value") {
          parent.asInstanceOf[js.Dynamic].value = mod.v.asInstanceOf[String]
        } else if (mod.a.name == "checked")
          parent.asInstanceOf[js.Dynamic].checked = mod.v.asInstanceOf[Boolean]
        else if (mod.a.name == "disabled")
          parent.asInstanceOf[js.Dynamic].disabled = mod.v.asInstanceOf[Boolean]
        else mod.applyTo(parent)
      }
      addSink(parent, sink)
    }
  }

  implicit class RxStyleModifier(val sig: Rx[_ <: StylePair[dom.Element, _]]) extends AnyVal {
    def toMod: Modifier = (parent: Element) => {
//      dom.console.log("hoho", parent)
      val sink = sig.mkObs { mod => mod.applyTo(parent) }
      addSink(parent, sink)
    }
  }

  implicit class RxDMapMap[X <: Product](val diffs: Rx[_ <: GenTraversableOnce[(String, X)]]) extends AnyVal {
    def dmapmap(fun: X => TypedTag[_ <: dom.Element]): Modifier = (parent: Element) => {
      val map = mutable.Map[String, dom.Element]()
      val sink = diffs.mkObs({ diffmap =>
        diffmap foreach { case (k,mk) =>
          (map remove k).foreach(oldelem =>
            removeChild(parent, oldelem))
          if (mk != null) {
            val newelem = fun(mk).render
            map += (k -> newelem)
            insertChild(parent, newelem)
          }
        }
      }, () => {
//        store.sample foreach { mk =>
//          val newelem = fun(mk).render
//          map += (mk.hashCode -> newelem)
//          insertChild(parent, newelem)
//        }
      })
      addSink(parent, sink)
    }
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
  private val sinkMap = new WeakMap[dom.Node, mutable.Set[Obs[_]]]()

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
 
  private def foreachChildSink(fc: dom.Element)(f: Obs[_] => Unit): Unit =
    collectChildSinks(fc).foreach(f)
  private def foreachSink(fc: dom.Element)(f: Obs[_] => Unit): Unit =
    (collectChildSinks(fc) ++ sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)

  private def isRooted(node: dom.Node): Boolean =
    dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]
}
