/** Created by david on 05.05.17. */

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

object TodoJS extends js.JSApp {

  class Task(title_ : String) extends VarOwner {
    val title: Variable[String] = mkVar(title_, "t")
    val done: Variable[Boolean] = mkVar(false, "d")
  }

  val todoTextColor: Variable[String] = new Variable("green", "col")
  val model: Store[Task, String] = new Store((name: String) => new Task(name), "model")

  val domView: (Task) => TypedTag[dom.html.Element] = Extras.lazyExtensionAttr { that =>
    val changeCtr = Extras.zip(that.title, that.done).fold(0)((state, event) => state + 1)

    val changed = new Channel[Unit]()
    val lastentries = that.title.snapshot(changed)
      .fold(List[String]())((state, event) => event :: state.take(10))

    li(
      Signal(if (that.done.get) `class` := "task done" else `class` := "task").toMod,

      input(`type` := "checkbox",
        Signal((if (that.done.get) checked else ""):Modifier).toMod,
        onchange := (() => that.done.transform(!_))),

      input(list:="datalist-"+that.hashCode(),
        Signal(value := that.title.get).toMod,
        Signal(style := "color:" + todoTextColor.get).toMod,
        oninput  := { ev: dom.Event => that.title.set(ev.target.asInstanceOf[dom.html.Input].value) },
        onchange := { ev: dom.Event => that.title.set(ev.target.asInstanceOf[dom.html.Input].value); changed.send(()) }),

      lastentries.map(it => datalist(
        id:="datalist-"+that.hashCode(),
        it.map(it => option(value := it)))).toMod,

      changeCtr.map(span(_)).toMod
    )
  }

//  val counter: (Task) => Modifier = Extras.lazyExtensionAttr(that =>
//    )

  override def main(): Unit = transact {
    val mapped2 = model
      .map({ it => it.count(!_.done.get) }, "notdone")
      .map({ it => span(if (it == 0) "no" else ""+it) }, "string")
    val mapped3 = model.map({ it => span(it.toList.map { task => task.title.get.length }.sum) }, "charsum")

    val log = textarea(id:="log").render

    val x = model.map( it => it.map(domView(_))).wrap(ul).toMod
    val todolist = model.map(_.isEmpty).map( isEmpty => {
      if (isEmpty) div(`class` := "info", "All done! :)")
      else x
    }, "tasklist").toMod

    insertChild(dom.document.body, div(

//      {
//        val clock = new Variable(scalajs.js.Date())
//
//        val id = scala.scalajs.js.timers.setInterval(1000) {
//          clock.set(scalajs.js.Date())
//        }
//
//        // scala.scalajs.js.timers.clearInterval(id)
//
//        Signal(div(
//          h1("Hello, world!"),
//          h2("It is ", clock.get))).toMod
//      },

      button("gen ten", onclick := (() =>
        for (i <- 0 to 10) model.create( "unique" + i))),
      button("del ten", onclick := (() =>
        model.remove(model.sample))),
      button("paint", onclick := (() =>
        log.value = drx.debug.stringit(collectAllSinks(dom.document.body)))),

      br(),
      h1("DO TODOS!"),
      span("There are ", mapped2.toMod, " todos left, " +
        "with a total description length of ", mapped3.toMod, "."),

      form(
        input(placeholder:= "enter new task here"),
        onsubmit := { e: dom.Event =>
          e.preventDefault()
          val inputElem = e.target.asInstanceOf[dom.html.Form].children(0).asInstanceOf[dom.html.Input]
          model.create(inputElem.value)
          inputElem.value = ""
        }),

      span("first"),
      todolist,
      span("second"),
      todolist,

      input(
        `type`:="button",
        model.map(_.isEmpty).map(isEmpty => (
          if (isEmpty) style:="display:none"
          else style:=""):Modifier, "button").toMod,
        value:= "remove all done todos",
        onclick:= { () =>
          model.remove(model.sample.filter(_.done.sample))
        }
      ),

      br(),
      br(),
      br(),
      br(),

      log

//      button(
//        style:="display: block", onclick := { () =>
//          val tmp = document.querySelector("#svg-container svg")
//          tmp.outerHTML = Viz(rx.printit(), { engine: "dot" })
//        }, "So be it!"),
//      div(id:="svg-container", svg()).render,

//      button("doit", onclick:= { () => drx.helper.printless() }),

    ).render)
  }
}
