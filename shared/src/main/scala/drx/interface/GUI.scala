package drx.interface

import scala.collection.mutable
import drx.{ScalaWeakMap, concreteplatform}
import drx.interface.DSL._

// Declarative.
// An interesting property of GUI Elements is, that can only exist once per page!
// This means we cannot pass them around like normal values as we wish. (referential transparency)
// So we avoid them by postponing creating Elements to the latest possible time,
// and mostly pass Blueprints around which describe how to build a node.

trait GUI[Element] {
  trait Mod { def applyTo(w: Element): Unit }
  def modGroup(ms: Mod*): Mod = w => ms.foreach(m => m.applyTo(w))

  trait Blueprint extends Mod {
    def render: Element
    override def applyTo(parent: Element): Unit = appendRaw(parent, this.render)
  }

  def getMarkedChildren(fc: Element): Seq[Element]
  def mark(it: Element)
  def unmark(it: Element)

  //def isRooted(w: Element): Boolean
  //def getParent(w: Element): Element
  def getChildren(w: Element): Seq[Element]

  def appendRaw(parent: Element, child: Element): Unit
  def replaceRaw(old: Element, next: Element): Unit
  def removeRaw(w: Element): Unit

  def disabled(b: Boolean): Mod
  def gap(i: Int): Mod
  def color(c: String): Mod
  def width(w: Double): Mod
  def height(h: Double): Mod
  def callback(f: Element => Unit): Mod
  def text(s: String): Mod
  def promptText(s: String): Mod
  def checked(b: Boolean): Mod

  def textOf(w: Element): String

  def vbox(ms: Mod*): Blueprint
  def hbox(ms: Mod*): Blueprint
  def flow(ms: Mod*): Blueprint
  def label(ms: Mod*): Blueprint
  def input(ms: Mod*): Blueprint
  def checkbox(ms: Mod*): Blueprint
  def button(ms: Mod*): Blueprint

  // for some subtypes of Modifier, implicit conversions aproximating
  //   Rx[Modifier] ==> Modifier

  implicit def tagToMod(sig: Val[Blueprint]): Mod = (parent: Element) => {
    var oldelem: Element = label(text("{{inittag}}")).render
    val sinkTag = sig.map { newmk =>
      val newelem = newmk.render
      replace(oldelem, newelem)
      oldelem = newelem
    }
    addSink(oldelem, sinkTag)
    appendRaw(parent, oldelem)
  }

  implicit def modToMod(sig: Val[Mod]): Mod = (parent: Element) => {
    val sinkMod = sig.map { mod =>
      mod.applyTo(parent)
    }
    addSink(parent, sinkMod)
  }

  implicit def seqToMod[X <: Element](diffs: Val[TraversableOnce[Blueprint]]
                                     ): Mod = (parent: Element) => {
    val sinkDiff = diffs.map { diffmap =>
      getChildren(parent).reverse foreach remove // TODO why is reverse necessary?
      diffmap foreach { tag =>
        val newelem = tag.render
        insertChild(parent, newelem) }
    }
    addSink(parent, sinkDiff)
  }

  implicit def deltaToMod[X <: Element](diffs: Val[TraversableOnce[(String, Polarized[Blueprint])]]
                                           ): Mod = (parent: Element) => {
    val map = mutable.Map[String, Element]()
    val sinkDiff = diffs.map { diffmap =>
      diffmap foreach { case (k, pol) =>
        val oldelem = map remove k
        pol match {
          case Pos(content) =>
            val newelem = content.render
            map(k) = newelem
            if (oldelem.isDefined) replace(oldelem.get, newelem)
            else insertChild(parent, newelem)
          case Neg(_) if oldelem.isDefined =>
            remove(oldelem.get)
          case _ =>
        }
      }
    }
    addSink(parent, sinkDiff)
  }

  def insertChild(parent: Element, elem: Element): Unit = {
    val placeholder: Element = label(text("{{insert}}")).render
    appendRaw(parent, placeholder)
    replace(placeholder, elem)
  }
  def remove(elem: Element): Unit = {
    getSinks(elem).foreach { case (elem, sink) => remSink(elem, sink) }
    removeRaw(elem)
  }

  private val sinkMap = concreteplatform.WeakMap[Element, mutable.Set[Val[_]]]().asInstanceOf[ScalaWeakMap[Element, mutable.Set[Val[_]]]]
  private def addSink(it: Element, obs: Val[_]): Unit = {
    val sinks = sinkMap.get(it).getOrElse {
      val tmp = mutable.Set[Val[_]]()
      sinkMap.set(it, tmp)
      mark(it)
      tmp
    }
    sinks += obs
  }
  private def remSink(it: Element, obs: Val[_]): Unit = {
    val sinks = sinkMap.get(it).get
    sinks -= obs
    if (sinks.isEmpty) unmark(it)
  }

//  private def foreachChildSink(fc: Element)(f: Val[_] => Unit): Unit =
//    getMarkedChildren(fc).flatMap(sinkMap.get(_).get).foreach(f)
//  private def foreachSink(fc: Element)(f: Val[_] => Unit): Unit =
//    (getMarkedChildren(fc).flatMap(sinkMap.get(_).get) ++
//      sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)
  def getSinks(elem: Element): Set[(Element, Val[_])] =
    getMarkedChildren(elem).flatMap(x => sinkMap.get(x).getOrElse(None.toIterable).map(y => (x, y))).toSet

  def init(top: Element): Unit =
    getEnableds.append { () => getSinks(top).map(_._2) }

  def replace(oldelem: Element, newelem: Element): Unit = transact {
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }

    //val rooted = isRooted(oldelem)
    //if (rooted) foreachChildSink(oldelem)(_.disable()) // stop unrooted sinks
    replaceRaw(oldelem, newelem)
    //if (rooted) foreachSink(newelem)(_.enable())       // start rooted sinks

    // why order stop, replace, start?
  }

  // TODO wrap use blueprint as owner?
//  def sFullName(labelText: Var[String], texts: Var[String]): Blueprint = {
//    val clicked = Var(0)
//    val first   = Var("")
//    val last    = Var("")
//    clicked.foreach { _ => first.set(""); last.set("") }
//    val full  = first.zip(last)
//      .map {case (f: String, l: String) => f +" "+ l }
//    clicked.foreach(x => texts set full.sample)
//
//    vbox(
//      labelText.map(x=>label(text(x))),
//      hbox(
//        input(textBi(first), promptText("first name"), width(.5)),
//        input(textBi(last),  promptText("last name"), width(.5))),
//      button(callback(_ => clicked.transform(_+1)), text("submit"),
//        Val(disabled(first.get.length<1 || last.get.length<1)))
//    )
//  }

  def checkedBi(sig: Var[Boolean]): Mod = modGroup(
      Val(checked(sig.get)),
      callback( _ => sig.transform(!_) )
  )

  def textBi(sig: Var[String]): Mod = modGroup(
    Val(text(sig.get)),
    callback { w => sig.set(textOf(w)) },
  )

  def sCommand(sig: String => Unit, m: Mod*): Blueprint = input((Seq[Mod](
    callback { w =>
      transact(sig(textOf(w)))
      text("").applyTo(w)
    },
  )++ m):_*)

//  def sClock(): Widget = {
//    val clock = Var(scalajs.js.Date())
//    val id = scala.scalajs.js.timers.setInterval(1000) { clock.set(scalajs.js.Date()) }
//    // scala.scalajs.js.timers.clearInterval(id)
//    div(tagToMod(Val(b("It is ", clock.get))), br, br)
//  }

//  def toggleDisplayNone(b: Boolean): Blueprint =
//    display:=(if (b) "none" else "")

}
