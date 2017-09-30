/** Created by david on 05.05.17. */

import drx._
import main.Task
import org.scalajs.dom.Element

import scala.language.implicitConversions

import scalatags.JsDom.all._
import scala.scalajs.js
import org.scalajs.dom

import scalatags.JsDom.TypedTag

object TodoJS extends js.JSApp {

  private val DATA_IS_REACTIVE = "data-is-reactive"

  def getObs(it: dom.Node): Sink[_] = it.asInstanceOf[js.Dynamic].drxObserverReference.asInstanceOf[Sink[_]]
  def setSink(it: dom.Element, obs: Sink[_]): Unit = {
    it.asInstanceOf[js.Dynamic].drxObserverReference = obs.asInstanceOf[js.Any]
    if (obs != null) it.setAttribute(DATA_IS_REACTIVE, "true")
    else             it.removeAttribute(DATA_IS_REACTIVE)
  }
  def isVisible(node: dom.Node): Boolean = dom.document.body.asInstanceOf[js.Dynamic].contains(node).asInstanceOf[Boolean]
  private def foreachObs(fc: dom.Element)(f: Sink[_] => Unit): Unit = {
    val list = fc.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (0 until list.length).foreach { i => f(getObs(list(i))) }
  }

  def replaceLastChild(medium: dom.Element, newelem: dom.Element): Unit = {
    val visible = isVisible(medium)
    if (visible) foreachObs(medium)(_.stop())
    medium.replaceChild(newelem, medium.lastChild)
    if (visible) foreachObs(medium)(_.start())
  }
  implicit class SignalToElement(val sig: Rx[_ <: dom.html.Element]) extends AnyVal {
    def drender: dom.html.Element = {
      val medium = span(span("{{init}}")).render
      val startObs: Sink[_] = sig.mkObs { newelem =>
        replaceLastChild(medium, newelem) // last and only child
      }
      setSink(medium, startObs)
      medium
    }
  }

  def replaceChild(medium: dom.Element, oldelem: dom.Element, newelem: dom.Element, sink: Sink[_]): Unit = {
    if (isVisible(medium)) {
      foreachObs(oldelem)(_.stop())
      foreachObs(newelem)(_.start())
    }
    setSink(oldelem, null)
    setSink(newelem, sink)
    medium.replaceChild(newelem, oldelem)
  }
  implicit class SignalToElement2(val sig: Rx[_ <: TypedTag[dom.html.Element]]) extends AnyVal {
    def drender: Modifier = (t: dom.Element) => {
      var oldelem: dom.Element = span("{{init}}").render
      lazy val sink: Sink[_] = sig.mkObs { mknew =>
        val newelem = mknew.render
        replaceChild(t, oldelem, newelem, sink)
        oldelem = newelem
      }
      setSink(oldelem, sink)
      t.appendChild(oldelem)
    }
  }

  def attrValue(sig: Rx[Modifier]): Modifier =
    (t: dom.Element) => sig.mkObs { mod =>
      mod.applyTo(t)
    }

//  implicit def styleValue[T: StyleValue](t: dom.Element, a: Style, signal: Signal[T]): Unit =
//      signal.foreach { value => implicitly[StyleValue[T]].apply(t, a, value) }

// for debugging / testing purposes
//  def checkConsistency(): Unit = {
//    val a: Set[Observer[_]] = collectAllObs()
//    val b: Set[Observer[_]] = drx.debug.transitivehullobservers(a)
//    if (a != b) println("mist: onlydom=" + (a -- b).map(_.id) + " | onlygraph=" + (b -- a).map(_.id))
//  }

  def collectAllObs(): Set[Sink[_]] = {
    val list = dom.document.body.querySelectorAll("["+DATA_IS_REACTIVE+"]")
    (for (i <- 0 until list.length) yield getObs(list(i))).toSet[Sink[_]]
  }

  override def main(): Unit = {
    grouped {
      object state extends VarOwner {
        val todoTextColor: Variable[String] = mkVar("green", "col")
        val model: Store[Task, String] = mkStore((name: String) => new Task(name), "model")
      }

      def dview(task: Task): Modifier = {
        println("run me")
        Signal(li(
          if (task.done.get) `class` := "task done" else `class` := "task",
          input(`type` := "checkbox",
            if (task.done.get) checked else "",
            onchange := { () => task.done.transform(!_) }),
          input(attrValue(Signal(value := task.title.get)),
            style := "color:" + state.todoTextColor.get,
            onchange := { ev: dom.Event => task.title.set(ev.target.asInstanceOf[dom.html.Input].value) }),
          task.folded.map(span(_)).drender
        )).drender
      }

//      val model = new Variable(List[Task](), "model")

      val mapped2 = state.model
        .map({ it => it.count(!_.done.get) }, "notdone")
        .map({ it => span(if (it == 0) "no" else ""+it) }, "string")
      val mapped3 = state.model.map({ it => span(it.toList.map { task => task.title.get.length }.sum) }, "charsum")

      val log = textarea(id:="log").render

      val todolist = Signal(
        if (state.model.get.isEmpty)
          div(`class` := "info", "All done! :)")
        else ul(state.model.get.map(dview).toList)
        , "tasklist")

      dom.document.body.appendChild(span("{{init}}").render)
      replaceLastChild(dom.document.body, div(

        button("gen ten", onclick := (() =>
          for (i <- 0 to 10) state.model.create( "unique" + i))),
        button("del ten", onclick := (() =>
          state.model.remove(state.model.sample))),
        button("paint", onclick := (() =>
          log.value = drx.debug.stringit(collectAllObs().toSet))),

        br(),
        h1("DO TODOS!"),
        span("There are ", mapped2.drender, " todos left, " +
          "with a total description length of ", mapped3.drender, "."),

        {
          val inputElem = input(placeholder:= "enter new task here").render
          form(inputElem, onsubmit := { e: dom.Event => e.preventDefault()
//            val input = e.target.asInstanceOf[dom.html.Form].children(0).asInstanceOf[dom.html.Input]
            println(inputElem.value)
            state.model.create(inputElem.value)
            inputElem.value = ""
          })
        },

        span("first"),
        todolist.drender,
        span("second"),
        todolist.drender,

        Signal(
          if (state.model.get.isEmpty)
            br()
          else input(
            `type`:="button",
            value:= "remove all done todos",
            onclick:= { () =>
              state.model.remove(state.model.sample.filter(item => item.done.sample))
//              model.transform( it => it.filter(item => !item.done.now))
            }
          )
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
