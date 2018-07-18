import platform.platform.WeakMap
import drx.{Rx, Sink, VarLike}
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node

import scala.collection.mutable
import scala.scalajs.js
import scalatags.generic.{AttrPair, StylePair}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import scala.language.implicitConversions

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

  def replaceChild(medium: dom.Element, oldelem: dom.Element, newelem: dom.Element, sink: Sink[_]): Unit = {
    if (isVisible(medium)) {
      foreachSink(oldelem)(_.stop())
      foreachSink(newelem)(_.start())
    }
    remSink(oldelem, sink)
    addSink(newelem, sink)
//    println(oldelem.outerHTML, "-->", newelem.outerHTML)
    medium.replaceChild(newelem, oldelem)
  }

  def insertChild(medium: dom.Element, elem: dom.Element): Unit = {
    if (isVisible(medium)) foreachSink(elem)(_.start())
    medium.appendChild(elem)
  }

  implicit class RxDMapMap[X <: drx.VarLike, CtorArgs](val store: drx.Store[X, CtorArgs]) extends AnyVal {
    def dmapmap(parentx: TypedTag[dom.Element],
                fun: X => TypedTag[dom.Element]): Modifier = { (t: Element) =>
      val parent = parentx.render
      val map = mutable.Map[Int, dom.Element]()
      val sink: Sink[_] = store.diffs.mkObs({ case (minus, plus) =>
        plus foreach { newmk =>
          val newelem = fun(newmk).render
          map += (newmk.hashCode -> newelem)
          if (isVisible(parent)) {
            foreachSink(newelem)(_.start())
            sinkMap.get(newelem).foreach(_.foreach(_.start()))
          }
          parent.appendChild(newelem)
        }
        minus foreach { newmk =>
          val oldelem = (map remove newmk.hashCode).get
          if (isVisible(parent)) {
            foreachSink(oldelem)(_.stop())
            sinkMap.get(oldelem).foreach(_.foreach(_.stop()))
          }
          parent.removeChild(oldelem)
        }
      })
      addSink(parent, sink)
      t.appendChild(parent)
    }
  }

  implicit class RxElemModifier(val sig: Rx[_ <: TypedTag[dom.Element]]) {
    private val realmod: (Element) => Unit = {(t: Element) =>
      var oldelem: dom.Element = span("{{init}}").render
      lazy val sink: Sink[_] = sig.mkObs { newmk =>
        val newelem = newmk.render
        replaceChild(t, oldelem, newelem, sink)
        oldelem = newelem
      }
      addSink(oldelem, sink)
      t.appendChild(oldelem)
    }
    def toMod: Modifier = t => realmod(t)
  }

  // TODO implement unmodifiers (unnapplyto) in scalatags?
  implicit class RxAttrModifier(val sig: Rx[_ <: AttrPair[dom.Element, _]]) extends AnyVal {
    def toMod: Modifier = (t: dom.Element) => {
      lazy val sink: Sink[_] = sig.mkObs { mod =>
        if (mod.a.name == "value")
          t.asInstanceOf[js.Dynamic].value = mod.v.asInstanceOf[String]
        else if (mod.a.name == "checked")
          t.asInstanceOf[js.Dynamic].checked = mod.v.asInstanceOf[Boolean]
        else if (mod.a.name == "disabled")
          t.asInstanceOf[js.Dynamic].disabled = mod.v.asInstanceOf[Boolean]
        else mod.applyTo(t)
      }
      addSink(t, sink)
    }
  }

  implicit class RxStyleModifier(val sig: Rx[_ <: StylePair[dom.Element, _]]) extends AnyVal {
    def toMod: Modifier = (t: dom.Element) => {
      lazy val sink: Sink[_] = sig.mkObs { mod => mod.applyTo(t) }
      addSink(t, sink)
    }
  }

  implicit def toMod1(sig: Rx[_ <: TypedTag[dom.Element]]): Modifier
    = RxElemModifier(sig).toMod
  implicit def toMod2(sig: Rx[_ <: StylePair[dom.Element, _]]): Modifier
    = RxStyleModifier(sig).toMod
  implicit def toMod3(sig: Rx[_ <: AttrPair[dom.Element, _]]): Modifier
    = RxAttrModifier(sig).toMod



//  implicit class RxStuff(val sig: Rx[_ <: Iterable[TypedTag[dom.Element]]]) extends AnyVal {
//    def wrap(wrapper: TypedTag[dom.html.Element]): drx.Rx[TypedTag[dom.Element]] =
//      sig.map(x => wrapper(x.toSeq))
//  }

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

//  implicit class RxElemModifier(val sig: Rx[_ <: TypedTag[dom.Element]]) {
//    private val realmod: (Element) => Node = drx.Extras.lazyExtensionAttr { t: Element =>
//      println("once ", t, sig.id)
//      val oldelem: dom.Element = span("{{init}}").render
//      lazy val sink: Sink[_] = sig.mkFold(oldelem){ (state, event) =>
//        val rendered = event.render
//        println(s"  run ${sink.id} -> ", rendered)
//        replaceChild(t, state, rendered, sink)
//        rendered
//      }
//      addSink(oldelem, sink)
//      dom.window.console.log(s"    create obs ${sink.id} for ${sig.id}")
//      t.appendChild(oldelem)
//    }
//    def toMod: Modifier = t => realmod(t)
//  }

//  implicit class RxElemsModifier(val sig: Rx[_ <: Seq[TypedTag[dom.Element]]]) {
//    private val realmod: (Element) => Node = drx.Extras.lazyExtensionAttr { t: Element =>
//      println("once ", t, sig.id)
//      var oldmk: TypedTag[dom.Element] = span("{{init}}")
//      var oldelem: dom.Element = oldmk.render
//      lazy val sink: Sink[_] = sig.mkObs { newmk =>
//        if (oldmk != newmk) {
//          val newelem = span(newmk).render
//          println(s"  run ${sink.id} -> ", newelem)
//          replaceChild(t, oldelem, newelem, sink)
//          oldelem = newelem
//          oldmk = newmk
//        }
//      }
//      addSink(oldelem, sink)
//      dom.window.console.log(s"    create obs ${sink.id} for ${sig.id}")
//      t.appendChild(oldelem)
//    }
//    def toMod: Modifier = t => realmod(t)
//  }

//  def attrValue(sig: Rx[Modifier]): Modifier =
//    (t: dom.Element) => sig.mkObs { mod =>
//      mod.applyTo(t)
//    }

//  implicit def styleValue[T: StyleValue](t: dom.Element, a: Style, signal: Signal[T]): Unit =
//      signal.foreach { value => implicitly[StyleValue[T]].apply(t, a, value) }







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

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }
}
