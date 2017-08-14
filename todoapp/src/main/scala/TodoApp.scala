/** Created by david on 05.05.17. */

import drx._
import main.Task

import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

object TodoApp extends js.JSApp {

  def replaceLastChild(medium: dom.Node, newelem: dom.Node): Unit = {
    forallObs(medium) { o => if (o.isActive) o.deactivate() }
    medium.replaceChild(newelem, medium.lastChild)
    if (dom.document.body.asInstanceOf[js.Dynamic].contains(medium).asInstanceOf[Boolean])
      grouped( forallObs(medium) { o => if (!o.isActive) o.activate() } )
  }

  implicit class SignalToElement(val sig: Signal[_ <: dom.html.Element]) extends AnyVal {
    def drender: dom.html.Element = {
      val medium = span(span("{{init}}")).render
      val obs = sig.onChange { newelem: dom.Node =>
        replaceLastChild(medium, newelem) // last and only child
        js.timers.setTimeout(100)(checkConsistency())
      }
      medium.asInstanceOf[js.Dynamic].drxObserverReference = obs.asInstanceOf[js.Any]
      medium.setAttribute("data-has-obs", obs.id)
      medium
    }
  }
  private def forallObs(fc: dom.Node)(f: Observer[_] => Unit) = fc match {
    case fcq: dom.Element     =>
      val list = fcq.querySelectorAll(":not(.well-hidden) [data-has-obs]")
      (0 until list.length).foreach { i => f(getObs(list(i))) }
    case _ /* TextNode */ => Unit
  }

  def getObs(it: dom.Node): Observer[_] = it.asInstanceOf[js.Dynamic].drxObserverReference.asInstanceOf[Observer[_]]

  // optionally, we could have type safe interfaces via weakmap
//  @js.native @js.annotation.JSGlobal
//  class WeakMap[Key <: js.Any, Value <: js.Any] extends js.Object {
//    def delete(key: Key): Unit = js.native
//    def has(key: Key): Boolean = js.native
//    def get(key: Key): js.UndefOr[Value] = js.native
//    def set(key: Key, value: Value): Unit = js.native
//  }
//  private val metadata = new WeakMap[dom.Node, drx.Observer[_]]()

  // for debugging / testing purposes
  def checkConsistency(): Unit = {
    val a: Set[Observer[_]] = collectAllObs()
    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
  }
  def collectAllObs(): Set[Observer[_]] = {
    val list = dom.document.body.querySelectorAll(":not(.well-hidden) [data-has-obs]")
    val x: IndexedSeq[Observer[_]] = for (
      i <- 0 until list.length
      if !list .apply(i).asInstanceOf[dom.html.Span].classList.contains("well-hidden")
    ) yield getObs(list(i))
    x.toSet
  }

  override def main(): Unit = {
    grouped {

      val color = new Var("red", "col")

      def dview(task: Task): dom.Element = Signal(li(
        if (task.done.get) `class` := "task done" else `class` := "task",
        input(`type` := "checkbox",
          if (task.done.get) checked else "",
          onchange := { ev: dom.Event => task.done.transform { it => !it } }),
        input(`class` := task.title.get,
          value := task.title.get,
          style := "color:" + color.get,
          onchange := { ev: dom.Event => task.title.set(ev.target.asInstanceOf[dom.html.Input].value) })
      ).render).drender

      val model = new Var(List[Task](), "model")

      val mapped = model
        .map({ it => it.count(!_.done.get) }, "notdone")
        .map({ it => if (it == 0) "no" else ""+it }, "string")

      val mapped2 = mapped
        .map({ it => span(it) }, "span")
        .map({ it => it.render }, "render")

      val mapped3 = model
        .map({ it => span(it.map { task => task.title.get.length }.sum).render }, "charsum")

  //    mapped.onChange(println)

      val log = textarea(id:="log").render

      dom.document.body.appendChild(span("{{init}}").render)
      replaceLastChild(dom.document.body, div(

        button("gen ten", onclick := { e: dom.Event =>
          for (i <- 0 to 10) model.transform( list => new Task("unique" + i) :: list)
          log.value = drx.debug.stringit(collectAllObs())
        }),
        button("del ten", onclick := { e: dom.Event =>
          model.set( List[Task]() )
          log.value = drx.debug.stringit(collectAllObs())
        }),
        button("paint", onclick := { e: dom.Event =>
          checkConsistency()
          log.value = drx.debug.stringit(collectAllObs())
        }),

        br(),
        h1("DO TODOS!"),
        span("There are ", mapped2.drender, " todos left, " +
          "with a total description length of ", mapped3.drender, "."),

        form(input(placeholder:= "enter new task here"), onsubmit := { ev: dom.Event => ev.preventDefault()
          val input = ev.target.asInstanceOf[dom.Element].children(0).asInstanceOf[dom.html.Input]
          model.transform { model => model ++ List(new Task(input.value)) }
          input.value = ""
        }),

        Signal(
          if (model.get.isEmpty)
            div(`class` := "info", "All done! :)").render
          else
            Signal(ul(model.get.map(dview)).render, "ul").drender
        , "tasklist").drender,

        Signal(
          if (model.get.isEmpty)
            br().render
          else input(
            `type`:="button",
            value:= "remove all done todos",
            onclick:= { () =>
              model.transform( it => it.filter(item => !item.done.now))
            }
          ).render
        ,"button").drender,

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

}
