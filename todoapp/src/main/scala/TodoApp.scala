/** Created by david on 05.05.17. */

import drx._
import main.Task

import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

object TodoApp extends js.JSApp {

  private val DATA_IS_REACTIVE = "data-is-reactive"

  def getObs(it: dom.Node): Callback[_] = it.asInstanceOf[js.Dynamic].drxObserverReference.asInstanceOf[Callback[_]]
  def setObs(it: dom.Node, obs: Callback[_]): Unit = it.asInstanceOf[js.Dynamic].drxObserverReference = obs.asInstanceOf[js.Any]
  def bodyContains(node: dom.Node): Boolean = dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]
  def replaceLastChild(medium: dom.Element, newelem: dom.Element): Unit = {
    foreachObs(medium)(_.stop())
    medium.replaceChild(newelem, medium.lastChild)
    if (bodyContains(medium)) foreachObs(medium)(_.start())
  }
  private def foreachObs(fc: dom.Element)(f: Callback[_] => Unit) = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (0 until list.length).foreach { i => f(getObs(list(i))) }
  }
  implicit class SignalToElement(val sig: Signal[_ <: dom.html.Element]) extends AnyVal {
    def drender: dom.html.Element = {
      val medium = span(span("{{init}}")).render
      val startObs: Callback[_] = sig.mkObs { newelem =>
        replaceLastChild(medium, newelem) // last and only child
//        js.timers.setTimeout(100)(checkConsistency())
      }
      setObs(medium, startObs)
      medium.setAttribute(DATA_IS_REACTIVE, "true")
      medium
    }
  }

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }

  def collectAllObs(): Set[Callback[_]] = {
    val list = dom.document.body.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length) yield getObs(list(i))).toSet[Callback[_]]
  }

  override def main(): Unit = {
    grouped {

      val color = new Variable("green", "col")

      def dview(task: Task): dom.Element = Signal(li(
        if (task.done.get) `class` := "task done" else `class` := "task",
        input(`type` := "checkbox",
          if (task.done.get) checked else "",
          onchange := { ev: dom.Event => task.done.transform { it => !it } }),
        input(`class` := task.title.get,
          value := task.title.get,
          style := "color:" + color.get,
          onchange := { ev: dom.Event => task.title.set(ev.target.asInstanceOf[dom.html.Input].value) }),
        task.folded.map(span(_).render).drender
      ).render).drender

      val model = new Variable(List[Task](), "model")
      val mapped2 = model
        .map({ it => it.count(!_.done.get) }, "notdone")
        .map({ it => span(if (it == 0) "no" else ""+it).render }, "string")
      val mapped3 = model.map({ it => span(it.map { task => task.title.get.length }.sum).render }, "charsum")

      val log = textarea(id:="log").render

      dom.document.body.appendChild(span("{{init}}").render)
      replaceLastChild(dom.document.body, div(

        button("gen ten", onclick := { e: dom.Event =>
          for (i <- 0 to 10) model.transform( list => new Task("unique" + i) :: list)
          log.value = drx.debug.stringit(collectAllObs().toSet)
        }),
        button("del ten", onclick := { e: dom.Event =>
          model.set( List[Task]() )
          log.value = drx.debug.stringit(collectAllObs().toSet)
        }),
        button("paint", onclick := { e: dom.Event =>
//          checkConsistency()
          log.value = drx.debug.stringit(collectAllObs().toSet)
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
