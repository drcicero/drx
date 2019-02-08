import drx.{Extras, Rx, Val, Var}
import org.scalajs.dom
import org.scalajs.dom.html.Div
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import RxDom._

object RxDomHelper {

  def rxFullName(labelText: Rx[String], texts: Var[String]): TypedTag[Div] = {
    val clicked = new Var[Int](0)
    val first   = new Var[String]("")
    val last    = new Var[String]("")
    clicked.observe { _ => first.set(""); last.set("") }
    val full  = Extras.zip(first, last)
      .map {case (f, l) => f +" "+ l }
    clicked.observe(x => texts set full.sample)

    div(
      label(labelText.map(span(_))), br,
      rxInput(first, placeholder:="first name", style:="display:inline;width:48%"),
      rxInput(last,  placeholder:="last name", style:="display:inline;width:48%"),
      rxButton(() => clicked.transform(_+1), value:="submit",
        Val(disabled:=first.get.length<1 || last.get.length<1))
    )
  }

  def rxCheckbox(sig: Var[Boolean], m:Modifier*) = input(
    tpe:="checkbox",
    Val(checked:=sig.get),
    onchange:=( () => sig.transform(!_)),
    m)

  def rxInput(sig: Var[String], m:Modifier*) = input(
    tpe:="text",
    Val(value:=sig.get),
    oninput:={ ev: dom.Event =>
      sig.set(ev.target.asInstanceOf[dom.html.Input].value) },
    m)

  def rxCommand(sig: String => Unit, m:Modifier*) = input(
    tpe:="text",
    onchange:={ e: dom.Event =>
      val inputElem = e.target.asInstanceOf[dom.html.Input]
      sig(inputElem.value)
      inputElem.value = ""
    },
    m)

  def rxButton(click: () => Unit, m:Modifier*) = input(
    tpe:="button",
    onclick:=( () => click() ),
    m)

  def rxClock() = {
    val clock = new Var(scalajs.js.Date())
    val id = scala.scalajs.js.timers.setInterval(1000) { clock.set(scalajs.js.Date()) }
    // scala.scalajs.js.timers.clearInterval(id)
    div(Val(b("It is ", clock.get)), br, br)
  }

  def toggleDisplayNone(b: Boolean) =
    display:=(if (b) "none" else "")

}
