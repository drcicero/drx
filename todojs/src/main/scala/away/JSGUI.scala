package away

import drx.interface.GUI
import org.scalajs.dom
import org.scalajs.dom.Element

import scala.scalajs.js

object JSGUI extends GUI[dom.Element] {

  override def isRooted(w: Element): Boolean = dom.document.body.asInstanceOf[js.Dynamic].contains(w).asInstanceOf[Boolean]
  override def getParent(w: Element): Element = w.parentNode.asInstanceOf[Element]
  override def getChildren(w: Element): Seq[Element] = (0 until w.childElementCount).map(w.children.item)
  override def appendRaw(w: Element, v: Element): Unit = w.appendChild(v)
  override def replaceRaw(w: Element, v: Element): Unit = w.parentNode.replaceChild(v, w)
  override def removeRaw(w: Element): Unit = w.parentNode.removeChild(w)

  override def textOf(w: Element): String = w match {
    case w: dom.html.Input => w.value
  }

  override def gap(i: Int): Mod = w =>
    w.setAttribute("style", w.getAttribute("style") + "; color:" + i)
  override def color(c: String): Mod = w =>
    w.setAttribute("style", w.getAttribute("style") + "; color:" + c)
  override def width(i: Double): Mod = w =>
    w.setAttribute("style", w.getAttribute("style") + "; width:" + (100*i) + "%")
  override def height(h: Double): Mod = w =>
    w.setAttribute("style", w.getAttribute("style") + "; height:" + (100*h) + "%")
  override def callback(f: dom.Element => Unit): Mod = w => {println(w); w match {
    case w: dom.html.Input => w.onchange = _ => f(w)
    case w: dom.html.Button => w.onclick = _ => f(w)
  }}
  override def disabled(b: Boolean): Mod = {
    case w: dom.html.Input => w.disabled = b
    case w: dom.html.Button => w.disabled = b
  }
  override def text(f: String): Mod = {
    case w: dom.html.Span => w.textContent = f
    case w: dom.html.Input => w.value = f
  }
  override def promptText(f: String): Mod = {
    case w: dom.html.Input => w.placeholder = f
  }
  override def checked(f: Boolean): Mod = {
    case w: dom.html.Input => w.checked = true
  }

  private def createElement(string: String, ms: Mod*) = new Blueprint {
    def render: Element = {
      val d = dom.document.createElement(string)
      ms.foreach{x => println(d.outerHTML); x.applyTo(d)}
      d
    }
  }
  override def button(ms: Mod*): Blueprint = createElement("button", ms:_*)
  override def vbox(ms: Mod*): Blueprint = createElement("div", ms:_*)
  override def hbox(ms: Mod*): Blueprint = createElement("span", ms:_*)
  override def flow(ms: Mod*): Blueprint = createElement("span", ms:_*)
  override def label(ms: Mod*): Blueprint = createElement("span", ms:_*)
  override def input(ms: Mod*): Blueprint = createElement("div", ms:_*)
  override def checkbox(ms: Mod*): Blueprint = createElement("div", ms:_*)

  private val DATA_IS_REACTIVE = "data-is-reactive"
  override def getMarkedChildren(fc: Element): Seq[Element] = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (0 until list.length).map { i => list.item(i) }.collect { case x: Element => x }
  }
  override def mark(it: Element): Unit = it.setAttribute(DATA_IS_REACTIVE, "true")
  override def unmark(it: Element): Unit = it.removeAttribute(DATA_IS_REACTIVE)
}
