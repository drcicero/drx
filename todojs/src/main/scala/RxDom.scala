import compat2.compat2.WeakMap
import drx.{Rx, Sink}
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node

import scala.collection.mutable
import scala.scalajs.js
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

/** Created by david on 15.09.17. */
object RxDom {
  private val DATA_IS_REACTIVE = "data-is-reactive"

  private val sinkMap = new WeakMap[dom.Node, mutable.Set[Sink[_]]]()

  def addSink(it: dom.Element, obs: Sink[_]): Unit = {
    val sinks = sinkMap.get(it).getOrElse {
      val tmp = mutable.Set[Sink[_]]()
      sinkMap.set(it, tmp)
      it.setAttribute(DATA_IS_REACTIVE, "true")
      tmp
    }
    sinks += obs
  }

  def remSink(it: dom.Element, obs: Sink[_]): Unit = {
    val sinks = sinkMap.get(it).get
    sinks -= obs
    if (sinks.isEmpty) it.removeAttribute(DATA_IS_REACTIVE)
  }

  def collectAllSinks(fc: dom.Element): Set[Sink[_]] = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length;
          y <- sinkMap.get(list(i)).getOrElse(mutable.Set()))
      yield y).toSet[Sink[_]]
  }

  private def foreachSink(fc: dom.Element)(f: Sink[_] => Unit): Unit =
    collectAllSinks(fc).foreach(f)

  def isVisible(node: dom.Node): Boolean =
    dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]

//  def replaceLastChild(medium: dom.Element, newelem: dom.Element): Unit = {
//    val visible = isVisible(medium)
//    if (visible) foreachSink(medium)(_.stop())
//    medium.replaceChild(newelem, medium.lastChild)
//    if (visible) foreachSink(medium)(_.start())
//  }
//  implicit class SignalToElement(val sig: Rx[_ <: dom.html.Element]) extends AnyVal {
//    def drender: dom.html.Element = {
//      val medium = span(span("{{init}}")).render
//      val startObs: Sink[_] = sig.mkObs { newelem =>
//        replaceLastChild(medium, newelem) // last and only child
//      }
//      setSink(medium, startObs)
//      medium
//    }
//  }

  def replaceChild(medium: dom.Element, oldelem: dom.Element, newelem: dom.Element, sink: Sink[_]): Unit = {
    if (isVisible(medium)) {
      foreachSink(oldelem)(_.stop())
      foreachSink(newelem)(_.start())
    }
    remSink(oldelem, sink)
    addSink(newelem, sink)
    medium.replaceChild(newelem, oldelem)
  }

  def insertChild(medium: dom.Element, elem: dom.Element): Unit = {
    if (isVisible(medium)) foreachSink(elem)(_.start())
    medium.appendChild(elem)
  }

  implicit class RxStuff(val sig: Rx[_ <: List[TypedTag[dom.html.Element]]]) extends AnyVal {
    def wrap(wrapper: TypedTag[dom.html.Element]): TypedTag[dom.html.Element] = {
      val x = wrapper()
      x
    }
  }

  implicit class RxElemModifier(val sig: Rx[_ <: TypedTag[dom.html.Element]]) {
    private val realmod: (Element) => Node = drx.Extras.lazyExtensionAttr { t: Element =>
      dom.window.console.log("once ", t, sig.id)
      var oldmk: TypedTag[dom.html.Element] = span("{{init}}")
      var oldelem: dom.Element = oldmk.render
      lazy val sink: Sink[_] = sig.mkObs { newmk =>
        if (oldmk != newmk) {
          val newelem = newmk.render
          dom.window.console.log(s"  run ${sink.id} -> ", newelem)
          replaceChild(t, oldelem, newelem, sink)
          oldelem = newelem
          oldmk = newmk
        }
      }
      addSink(oldelem, sink)
      dom.window.console.log(s"    create obs ${sink.id} for ${sig.id}")
      t.appendChild(oldelem)
    }
    def toMod: Modifier = t => realmod(t)
  }

//  implicit class RxModListModifier(val sig: Rx[_ <: List[Modifier]]) {
//    private val realmod = drx.Extras.lazyExtensionAttr { t: Element =>
//      lazy val sink: Sink[_] = sig.mkObs { mod =>
//        dom.window.console.log(s"  run ${sink.id} -> ", t, mod.toString)
//        mod.applyTo(t)
//      }
//      dom.window.console.log(s"    create obs ${sink.id} for ${sig.id}")
//      addSink(t, sink)
//    }
//    def toMod: Modifier = t => realmod(t)
//  }

  // TODO implement unmodifiers (unnapplyto) in scalatags
  implicit class RxAttrModifier(val sig: Rx[_ <: Modifier]) extends AnyVal {
    def toMod: Modifier = (t: dom.Element) => {
      lazy val sink: Sink[_] = sig.mkObs { mod =>
        dom.window.console.log(s"  run ${sink.id} -> ", t, mod.toString)
        mod.applyTo(t)
      }
      dom.window.console.log(s"    create obs ${sink.id} for ${sig.id}")
      addSink(t, sink)
    }
  }

//  def attrValue(sig: Rx[Modifier]): Modifier =
//    (t: dom.Element) => sig.mkObs { mod =>
//      mod.applyTo(t)
//    }

//  implicit def styleValue[T: StyleValue](t: dom.Element, a: Style, signal: Signal[T]): Unit =
//      signal.foreach { value => implicitly[StyleValue[T]].apply(t, a, value) }

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }
}
