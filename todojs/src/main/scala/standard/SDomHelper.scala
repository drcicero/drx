package standard

import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.html.{Div, Input}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic.{AttrPair, StylePair}

import drx.interface.DSL._
import SDom._

object SDomHelper {

  // Declarative.
  // An interesting property of Dom.Node is, that they can only have one parent!
  // This means we cannot pass them around like normal values as we wish.
  // So we avoid them by postponing renderind to dom.Nodes to the latest possible time,
  // and mostly pass TypedTags around which describe how to build a node.

  def sFullName(labelText: Var[String], texts: Var[String]): TypedTag[Div] = {
    val clicked = Var(0)
    val first   = Var("")
    val last    = Var("")
    clicked.foreach { _ => first.set(""); last.set("") }
    val full  = first.zip(last)
      .map {case (f: String, l: String) => f +" "+ l }
    clicked.foreach(x => texts set full.sample)

    div(
      label(labelText.map(span(_))), br,
      sInput(first, placeholder:="first name", style:="display:inline;width:48%"),
      sInput(last,  placeholder:="last name", style:="display:inline;width:48%"),
      sButton(() => clicked.transform(_+1), value:="submit",
        attrToMod(Val(disabled:=first.get.length<1 || last.get.length<1)))
    )
  }

  def sCheckbox(sig: Var[Boolean], m:Modifier*): TypedTag[Input] = input(
    tpe:="checkbox",
    attrToMod(Val(checked:=sig.get)),
    onchange:=( () => sig.transform(!_)),
    m)

  def sInput(sig: Var[String], m:Modifier*): TypedTag[Input] = input(
    tpe:="text",
    attrToMod(Val(value:=sig.get)),
    oninput:={ ev: dom.Event =>
      sig.set(ev.target.asInstanceOf[dom.html.Input].value) },
    m)

  def sCommand(sig: String => Unit, m:Modifier*): TypedTag[Input] = input(
    tpe:="text",
    onchange:={ e: dom.Event =>
      val inputElem = e.target.asInstanceOf[dom.html.Input]
      transact(sig(inputElem.value))
      inputElem.value = ""
    },
    m)

  def sButton(click: () => Unit, m:Modifier*): TypedTag[Input] = input(
    tpe:="button",
    onclick:=( () => transact(click()) ),
    m)

  def sClock(): TypedTag[Div] = {
    val clock = Var(scalajs.js.Date())
    val id = scala.scalajs.js.timers.setInterval(1000) { clock.set(scalajs.js.Date()) }
    // scala.scalajs.js.timers.clearInterval(id)
    div(tagToMod(Val(b("It is ", clock.get))), br, br)
  }

  def toggleDisplayNone(b: Boolean): StylePair[Element, String] =
    display:=(if (b) "none" else "")

}
