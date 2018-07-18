/** Created by david on 05.05.17. */

// TODO display errors messages. common errors are:
// TODO * println -> toString -> leading .sample() inside an evaluation
// TODO * folds must be toplevel or inside Extra.lazyExtensionAttr blocks...?

import RxDom._
import drx._

import scalatags.JsDom.all.{checked, value}
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.DataList

import scala.scalajs.js
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import scala.language.implicitConversions

object TodoJS {

  class Task(title_ : String) extends VarOwner {
    val title: Variable[String] = mkVar(title_, "t")
    val done: Variable[Boolean] = mkVar(false, "d")
    override def toString: String = title.sample + " " + done.sample
  }

  val todoTextColor: Variable[String] = new Variable("green", "col")
  val model: Store[Task, String] = new Store((name: String) => new Task(name), "model")

  def main(args: Array[String]): Unit = transact {
    // in
    val clearDoneTodos = new Channel[Unit]
    val makeNewTodo    = new Channel[String]
    val fullName       = new Variable("")

    // out
    makeNewTodo    observe (value => model.create(value))
    clearDoneTodos observe (_     => model.remove(model.sample.filter(it =>
      it.done.sample || it.title.sample.isEmpty)))

    // signals
    val todotext = model
      .map({ it => it.count(!_.done.get) }, "notdone")
      .map({ it => span(if (it == 0) "are no active todos"
                        else if (it == 1) "is 1 active todo"
                        else "are " + it + " active todos") }, "string")
    val textlen = model
      .map({ it => span(it.toList.map { task => task.title.get.length }.sum) }, "charsum")
    val todolistlist = span(model.dmapmap(ul, rxTask))
    val todolist = model map ( lst =>
      if (lst.isEmpty) div(cls:="info", "All done! :)")
      else todolistlist, "tasklist")

    val log = textarea(id:="log").render

    val obj = div(
//      rxClock(), br,
//      rxFullName(Signal("So be it!"), fullName), br,
//      "Hello ", fullName.map(span(_)), "!", br,

      h1("DO TODOS!"),
      rxCommand(makeNewTodo, placeholder:="enter new todo here"),

      div(todolist, style:="display:inline-block; width:49%"),
      div(style:="display:inline-block; width:2%"),
      div(todolist, style:="display:inline-block; width:49%"),

//      span(model.diffs.map({ case (minus, plus) => span(
//        span("-- (", display:="inline-block", padding:=".5em"), minus.map {task =>
//          Signal(span(task.title.get, display:="inline-block", padding:=".5em")) }.toSeq,
//        span(") ++ (", display:="inline-block", padding:=".5em"), plus .map {task =>
//          Signal(span(task.title.get, display:="inline-block", padding:=".5em")) }.toSeq,
//        span(")", display:="inline-block", padding:=".5em"))
//      })),

      p("There ", todotext, " left, " +
        "with a total description length of ", textlen, "."),
      rxButton(
        Signal("remove all done todos"), clearDoneTodos,
        model.map(_.filter(_.done.get).isEmpty).map(toggleDisplayNone)),

      br, br, br,

      p("debug:"),
      button("gen ten", onclick:=(() =>
        model.creates( for (i <- 0 to 10) yield "unique" + i ))),
      button("del ten", onclick:=(() =>
        model.remove(model.sample))),
      button("paint", onclick:=(() =>
        log.value = drx.debug.stringit(collectAllSinks(dom.document.body)))),
      log

//      button(
//        style:="display:block", onclick:={ () =>
//          val tmp = document.querySelector("#svg-container svg")
//          tmp.outerHTML = Viz(rx.printit(), { engine: "dot" })
//        }, "So be it!"),
//      div(id:="svg-container", svg()).render,

//      button("doit", onclick:={ () => drx.helper.printless() }),
    )

    insertChild(dom.document.body, obj.render)
  }


  val rxTask: (Task) => TypedTag[dom.html.Element] = Extras.lazyExtensionAttr { that =>
    val changeCtr = Extras.zip(that.title, that.done).fold(0)((state, event) => state + 1)

    val changed = new Channel[Unit]()
    changed observe (_ => model.remove(model.sample.filter(it => it.title.sample.isEmpty)))
    val lastentries = that.title.snapshot(changed)
      .fold(List[String]())((state, event) => event :: state.take(10))

    li(
      Signal(cls:=(if (that.done.get) "task done" else "task")),

      rxCheckbox(that.done),

      rxInput(that.title,
        Signal(color:=todoTextColor.get),
        list:="datalist-"+that.hashCode(),
        onchange:=( () => changed.send(()) )),

      lastentries.map(it => datalist(
        id:="datalist-"+that.hashCode(),
        it.map(it => option(value:=it)))),

      changeCtr.map(span(_))
    )
  }

  def rxFullName(labelText: Rx[String], texts: Variable[String]) = {
    val clicked = new Channel[Unit]
    val first   = new Variable[String]("")
    val last    = new Variable[String]("")
    clicked.observe { _ => first.set(""); last.set("") }
    val full  = Extras.zip(first, last)
      .map {case (first, last) => first +" "+ last }
    full.snapshot(clicked).observe(texts.set)

    div(
      label(labelText.map(span(_))),
      rxInput(first, placeholder:="first name"),
      rxInput(last,  placeholder:="last name"),
      rxButton(Signal("submit"), clicked,
        Signal(disabled:=first.get.length<1 || last.get.length<1))
    )
  }

  def rxCheckbox(sig: Variable[Boolean], m:Modifier*) = input(
    tpe:="checkbox",
    Signal(checked:=sig.get),
    onchange:=( () => sig.transform(!_)),
    m)

  def rxInput(sig: Variable[String], m:Modifier*) = input(
    tpe:="text",
    Signal(value:=sig.get),
    oninput:={ ev: dom.Event =>
      sig.set(ev.target.asInstanceOf[dom.html.Input].value) },
    m)

  def rxCommand(sig: Channel[String], m:Modifier*) = input(
    tpe:="text",
    Signal(value:=sig.get), 
    onchange:={ e: dom.Event =>
      val inputElem = e.target.asInstanceOf[dom.html.Input]
      sig.send(inputElem.value)
      inputElem.value = ""
    },
    m)

  def rxButton(sig: Rx[String], ch: Channel[Unit], m:Modifier*) = input(
    tpe:="button",
    Signal(value:=sig.get),
    onclick:=( () => ch.send(()) ),
    m)

  def rxClock() = {
    val clock = new Variable(scalajs.js.Date())
    val id = scala.scalajs.js.timers.setInterval(1000) { clock.set(scalajs.js.Date()) }
    // scala.scalajs.js.timers.clearInterval(id)
    div(Signal(b("It is ", clock.get)), br, br)
  }

  def toggleDisplayNone(b: Boolean) =
    display:=(if (b) "none" else "")
}
