import platform.platform.WeakMap
import drx.{Rx, Sink, VarLike}

import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node

import scalatags.generic.{AttrPair, StylePair}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import scala.collection.mutable
import scala.scalajs.js
import scala.language.implicitConversions

// TODO never .render followed by appendChild. Then the component was not activated... Sorry

/** Created by david on 15.09.17. */
object RxDom {
  // for some subtypes of Modifier, implicit conversions aproximating
  //   Rx[Modifier] ==> Modifier
  implicit def toMod1(sig: Rx[_ <: TypedTag[dom.Element]]): Modifier
    = RxElemModifier(sig).toMod
  implicit def toMod2(sig: Rx[_ <: StylePair[dom.Element, _]]): Modifier
    = RxStyleModifier(sig).toMod
  implicit def toMod3(sig: Rx[_ <: AttrPair[dom.Element, _]]): Modifier
    = RxAttrModifier(sig).toMod

  def insertChild(parent: dom.Element, elem: dom.Element): Unit = {
    parent.appendChild(elem)
    if (isRooted(parent)) foreachSink(elem)(_.start())
//    println("    a " + elem.outerHTML)
  }

  def replaceChild(parent: dom.Element, oldelem: dom.Element, newelem: dom.Element): Unit = {
//    println("    r " + oldelem.outerHTML)
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }
    parent.replaceChild(newelem, oldelem) // root newelem
    if (isRooted(parent)) {
      foreachChildSink(oldelem)(_.stop()) // stop unrooted sinks
      foreachSink(newelem)(_.start())     // start rooted sinks
    }
//    println("    i " + newelem.outerHTML)
  }

  def removeChild(parent: dom.Element, elem: dom.Element): Unit = {
    parent.removeChild(elem)
    if (isRooted(parent)) foreachSink(elem)(_.stop())
  }

  implicit class RxElemModifier(val sig: Rx[_ <: TypedTag[dom.Element]]) {
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
        if (mod.a.name == "value")
          parent.asInstanceOf[js.Dynamic].value = mod.v.asInstanceOf[String]
        else if (mod.a.name == "checked")
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
      val sink = sig.mkObs { mod => mod.applyTo(parent) }
      addSink(parent, sink)
    }
  }

  implicit class RxDMapMap[X <: drx.VarLike, CtorArgs](val store: drx.Store[X, CtorArgs])
  extends AnyVal {
    def dmapmap(fun: (X) => TypedTag[_ <: dom.Element]): Modifier = (parent: Element) => {
      val map = mutable.Map[Int, dom.Element]()
      val sink = store.diffs.mkObs({ case (minus, plus) =>
        plus foreach { newmk =>
          val newelem = fun(newmk).render
          map += (newmk.hashCode -> newelem)
          insertChild(parent, newelem)
        }
        minus foreach { oldmk =>
          val oldelem = (map remove oldmk.hashCode).get
          removeChild(parent, oldelem)
        }
      }, () => {
        store.sample foreach { newmk =>
          val newelem = fun(newmk).render
          map += (newmk.hashCode -> newelem)
          insertChild(parent, newelem)
        }
      })
      addSink(parent, sink)
    }
  }



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
//      val oldelem: dom.Element = span("{{init}}").render
//      lazy val sink: Sink[_] = sig.mkFold(oldelem){ (state, event) =>
//        val rendered = event.render
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
//      var oldmk: TypedTag[dom.Element] = span("{{init}}")
//      var oldelem: dom.Element = oldmk.render
//      lazy val sink: Sink[_] = sig.mkObs { newmk =>
//        if (oldmk != newmk) {
//          val newelem = span(newmk).render
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



//  def replaceLastChild(parent: dom.Element, newelem: dom.Element): Unit = {
//    val visible = isRooted(parent)
//    if (visible) foreachSink(parent)(_.stop())
//    parent.replaceChild(newelem, parent.lastChild)
//    if (visible) foreachSink(parent)(_.start())
//  }
//  implicit class SignalToElement(val sig: Rx[_ <: dom.html.Element]) extends AnyVal {
//    def drender: dom.html.Element = {
//      val parent = span(span("{{init}}")).render
//      val startObs: Sink[_] = sig.mkObs { newelem =>
//        replaceLastChild(parent, newelem) // last and only child
//      }
//      setSink(parent, startObs)
//      parent
//    }
//  }

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }


  private val DATA_IS_REACTIVE = "data-is-reactive"
  private val sinkMap = new WeakMap[dom.Node, mutable.Set[Sink[_]]]()

  private def addSink(it: dom.Element, obs: Sink[_]): Unit = {
    val sinks = sinkMap.get(it).getOrElse {
      val tmp = mutable.Set[Sink[_]]()
      sinkMap.set(it, tmp)
      it.setAttribute(DATA_IS_REACTIVE, "true")
      tmp
    }
    sinks += obs
  }

  private def remSink(it: dom.Element, obs: Sink[_]): Unit = {
    val sinks = sinkMap.get(it).get
    sinks -= obs
    if (sinks.isEmpty) it.removeAttribute(DATA_IS_REACTIVE)
  }

  def collectChildSinks(fc: dom.Element): Set[Sink[_]] = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length;
          y <- sinkMap.get(list(i)).getOrElse(mutable.Set()))
      yield y).toSet[Sink[_]]
  }
 
  private def foreachChildSink(fc: dom.Element)(f: Sink[_] => Unit): Unit =
    collectChildSinks(fc).foreach(f)
  private def foreachSink(fc: dom.Element)(f: Sink[_] => Unit): Unit =
    (collectChildSinks(fc) ++ sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)

  private def isRooted(node: dom.Node): Boolean =
    dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]
}
