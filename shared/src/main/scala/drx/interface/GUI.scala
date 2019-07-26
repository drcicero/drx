package drx.interface

import drx.concreteplatform
import drx.interface.DSL.{Polarized, Val}

import scala.collection.mutable
import drx.interface.DSL._

trait GUI[Element] {
  trait Mod { def applyTo(w: Element): Unit }
  trait Blueprint extends Mod {
    def render: Element
    override def applyTo(parent: Element): Unit = appendRaw(parent, this.render)
  }

  def getMarkedChildren(fc: Element): Seq[Element]
  def mark(it: Element)
  def unmark(it: Element)

  def isRooted(w: Element): Boolean
  def getParent(w: Element): Element
  def getChildren(w: Element): Seq[Element]
  def appendRaw(parent: Element, child: Element): Unit
  def replaceRaw(old: Element, next: Element): Unit
  def removeRaw(w: Element): Unit

  def disabled(b: Boolean): Mod
  def gap(i: Double): Mod
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
      println("test " + newmk)
      val newelem = newmk.render
      replace(oldelem, newelem)
      oldelem = newelem
    }
    addSink(oldelem, sinkTag)
    insertChild(parent, oldelem)
  }

  implicit def modToMod(sig: Val[Mod]): Mod = (parent: Element) => {
    addSink(parent, sig.map(_.applyTo(parent)))
  }

  implicit def seqToMod[X <: Element](diffs: Val[TraversableOnce[(String, Blueprint)]]
                                     ): Mod = (parent: Element) => {
    val sinkDiff = diffs.map { diffmap =>
      getChildren(parent).reverse foreach remove // TODO why is reverse necessary?
      diffmap foreach { case (k, tag) =>
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
        if (pol.polarity) {
          val newelem = pol.content.render
          map(k) = newelem
          if (oldelem.isDefined) replace(oldelem.get, newelem)
          else insertChild(parent, newelem)
        } else if (oldelem.isDefined) {
          remove(oldelem.get)
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
    val placeholder: Element = label(text("{{delete}}")).render
    replace(elem, placeholder)
    removeRaw(placeholder)
  }

  private val sinkMap = concreteplatform.WeakMap[Element, mutable.Set[Val[_]]]()

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

  private def foreachChildSink(fc: Element)(f: Val[_] => Unit): Unit =
    getMarkedChildren(fc).flatMap(sinkMap.get(_).get).foreach(f)
  private def foreachSink(fc: Element)(f: Val[_] => Unit): Unit =
    (getMarkedChildren(fc).flatMap(sinkMap.get(_).get) ++
      sinkMap.get(fc).getOrElse(mutable.Set())).foreach(f)

  def replace(oldelem: Element, newelem: Element): Unit = {
    sinkMap.get(oldelem).getOrElse(Seq()).foreach { sink => // transfer sinks from old to new elem
      remSink(oldelem, sink)
      addSink(newelem, sink)
    }

    val rooted = isRooted(oldelem)
    if (rooted) foreachChildSink(oldelem)(_.forceStop()) // stop unrooted sinks
    replaceRaw(oldelem, newelem)
    if (rooted) foreachSink(newelem)(_.forceStart()) // start rooted sinks

    // why order stop, replace, start?
  }

  // Declarative.
  // An interesting property of Dom.Node is, that they can only have one parent!
  // This means we cannot pass them around like normal values as we wish.
  // So we avoid them by postponing renderind to dom.Nodes to the latest possible time,
  // and mostly pass TypedTags around which describe how to build a node.

  def sFullName(labelText: Var[String], texts: Var[String]): Blueprint = {
    val clicked = Var(0)
    val first   = Var("")
    val last    = Var("")
    clicked.foreach { _ => first.set(""); last.set("") }
    val full  = first.zip(last)
      .map {case (f: String, l: String) => f +" "+ l }
    clicked.foreach(x => texts set full.sample)

    vbox(
      labelText.map(x=>label(text(x))),
      hbox(
        sInput(first, promptText("first name"), width(.5)),
        sInput(last,  promptText("last name"), width(.5))),
      sButton(() => clicked.transform(_+1), text("submit"),
        Val(disabled(first.get.length<1 || last.get.length<1)))
    )
  }

  def sCheckbox(sig: Var[Boolean], m: Mod*): Blueprint = checkbox((Seq[Mod](
    Val(checked(sig.get)),
    callback( _ => sig.transform(!_) )
  )++ m):_*)

  def sInput(sig: Var[String], m: Mod*): Blueprint = input((Seq[Mod](
    Val(text(sig.get)),
    callback { w => sig.set(textOf(w)) },
  )++ m):_*)

  def sCommand(sig: String => Unit, m: Mod*): Blueprint = input((Seq[Mod](
    callback { w =>
      transact(sig(textOf(w)))
      text("").applyTo(w)
    },
  )++ m):_*)

  def sButton(click: () => Unit, m: Mod*): Blueprint = button((Seq[Mod](
    callback( w => transact(click()) ),
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
