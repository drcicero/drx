//package main
//
///** Created by david on 05.05.17. */
//
//import drx._
//import Render._
//
//import scalatags.JsDom.all._
//
//import scala.scalajs.js.JSApp
//import org.scalajs.dom
//import org.scalajs.dom.raw.Node
//import org.scalajs.dom.{Element, Event, document}
//
//object JSMain extends JSApp {
//
//  private def drender(sig: Signal[dom.html.Element]): dom.html.Element = {
//    def disconnectObservers(fc: Node) = fc match {
//      case fcq: Element     => {
//        val list = fcq.querySelectorAll("[data-id]")
//        (0 until list.length).foreach { i =>
//          val itemid = list(i).asInstanceOf[Element].getAttribute("data-obs-id")
//          if (itemid != "lolipop") drx.debugObs.get(itemid) match {
//            case None    => throw new RuntimeException("double free " + itemid)
//            case Some(x) => x.disconnect()
//          }
//        }
//      }
//      case _ /* TextNode */ => Unit
//    }
//
//    val medium = scalatags.JsDom.all.span().render
//    medium.appendChild(document.createTextNode("{{init}}"))
//    val obs = sig.onChange({ newelem: dom.Node =>
//      val fc = medium.firstChild
//      disconnectObservers(fc)
//      medium.replaceChild(newelem, fc)
//    }, onDisconnect = { () =>
//      medium.removeAttribute("data-obs-id")
//    })
//    medium.setAttribute("data-obs-id", ""+obs.id)
//    medium
//  }
//
//  private def dview(task: Task): dom.Element = {
//    drender(new Signal({ () =>
//      li(
//        if (task.done.get) `class` :="task done" else `class` :="task",
//        input(`type` := "checkbox", if (task.done.get) checked else "",
//          onchange := { ev: dom.Event => task.done.transform { it => !it }
//          }),
//        input(`class` := task.title.get, value := task.title.get,
//          onchange := { ev: dom.Event => task.title.set(ev.target.asInstanceOf[dom.html.Input].value) })
//      ).render
//    }))
//  }
//
//  override def main(): Unit = {
//
//    val model = new Var(List[Task](), "model")
//    val mapped = model
//      .map({ it => it.length }, "length")
//      .map({ it => if (it == 0) "No" else ""+it }, "string")
//
//    val mapped2 = mapped
//      .map({ it => span(it) }, "span")
//      .map({ it => it.render.asInstanceOf[dom.html.Element] }, "render")
//
//    mapped.onChange(println)
//
//    document.body.appendChild(div(
//
//      h1("DO TODOs!"),
//      span("Therer are ", drender(mapped2), " todos left"),
//
//      form(onsubmit := { ev: Event =>
//          ev.preventDefault()
//          val input = ev.target.asInstanceOf[dom.Element].children(0).asInstanceOf[dom.html.Input]
//          model.transform { model => model ++ List(new Task(input.value)) }
//          input.value = ""
//        },
//        input(placeholder:= "enter new task here")
//      ),
//
//      drender(new Signal(() =>
//        if (model.get.isEmpty)
//          div(`class` := "info", "All done! :)").render
//        else
//          drender(new Signal(() => ul(model.get.map { it => dview(it) }).render))
//      )),
//
//      drender(new Signal(() =>
//        if (model.get.isEmpty)
//          br().render
//        else input(
//          `type`:="button",
//          value:= "remove all done todos",
//          onclick:= { () =>
//            model.transform( it => it.filter(item => !item.done.now))
//          }
//        ).render)
//      ),
//
//      br(),
//      br(),
//      br(),
//
////    button(
////      style:="display: block", onclick: { () =>
////        val tmp = document.querySelector("#svg-container svg")
////        tmp.outerHTML = Viz(rx.printit(), { engine: "dot" })
////      }
////    }, "So be it!"),
////    div(id:="svg-container", svg())
//
//      br(),
//
//      button("doit", onclick:= { () => drx.helper.printless() }),
//      div(id:="svg-container", div())
//
//    ).render)
//  }
//
//}
