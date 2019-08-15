package old

import drx.graph.{Scan, Val, Var}
import drx.{Extras, debug}
import main.{Task, Todolist}
import old.RxDom._
import old.RxDomHelper._
import org.scalajs.dom
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.collection.mutable
import scala.scalajs.js

object AppTodo {

  val todoTextColor: Var[String] = Var("green")

  def main(): Unit = {
    val svg_container = dom.document.querySelector("#svg-container")
    val slider = input(tpe:="range", min:=0, max:=0).render
    val content = div.render
    svg_container.appendChild(slider)
    svg_container.appendChild(content)
    val items = mutable.Buffer[(String, String)]()
    slider.onchange = { e =>
      val item = items(slider.valueAsNumber)
      content.innerHTML = item._1 + "<br>" +
        dom.window.asInstanceOf[js.Dynamic]
          .Viz(item._2, Map("engine" -> "dot")).asInstanceOf[String]
    }
    debug.hook = {str =>
      val string = drx.debug.stringit(collectChildSinks(dom.document.body))
      // val idx = slider.max.toInt + 1
      slider.max = items.size.toString
      items += str -> string
    }

//    transact {
//      model.create((ThreadLocalRandom.current().nextInt().toString, "milk", false))
//      model2.create((ThreadLocalRandom.current().nextInt().toString, "cheese", false))
//      model.create((ThreadLocalRandom.current().nextInt().toString, "choco", false))
//      model.create((ThreadLocalRandom.current().nextInt().toString, "honey", false))
//    }

//    drx.Network.pub(model.diffs, "todos")
//    val allOtherModels: Rx[(ClientID, Map[String, Task])] =
//      drx.Network.sub[Map[String, Task]](Map(), "todos")
//        .scan(("", Map[String, Task]())){ (state, event) => (event._1, (state._2 ++ event._2) filter { _._2 != null }) }
//    drx.Network.startHeartbeat()

    val todotext = Todolist.text.map(span(_))
    val textlen = Todolist.len.map(span(_))

    val todolist = div(
      ul(Todolist.model.diffs.dmapmap(rxTask)),
      div(Todolist.model.aggregate.map(lst => if (lst.isEmpty) cls:="info" else cls:="hidden"), "All done! :)"))

    val obj = div(
      h1("DO TODOS! "/*, drx.Network.localId*/),
      rxCommand(Todolist.addNewTodo, placeholder:="enter new todo here"),

//      div(todolist),
      div(/*drx.Network.localId, */todolist, style:="display:inline-block; width:48%"),
      div(style:="display:inline-block; width:4%"),
//      div(todolist, style:="display:inline-block; width:48%"),
//      div(style:="display:inline-block; width:2%"),
//      div(allOtherModels.map(x => span(x._1)), todolist2, style:="display:inline-block; width:48%"),

      p("There ", todotext, " left, " +
        "with a total description length of ", textlen, "."),
      rxButton(
        Todolist.removeDoneTodos, value:="remove all done todos",
        Todolist.model.aggregate.map(!_.exists(_._2.done.get)).map(toggleDisplayNone)),
    )
//    transact { // TODO why transact?
    replaceChild(dom.document.body, dom.document.body.lastElementChild, obj.render)
//    }

  }

  val rxTask: Task => JsDom.TypedTag[dom.html.Element] =
    Extras.lazyExtAttr { that =>
      val changeCtr = Scan(0){ prev => that.title.get; that.done.get; prev + 1 }

      val changed = Var[Boolean](false)
      changed foreach (_ => Todolist.removeEmptyTodos())
      val lastentries = Scan(List[String]()){ prev =>
        changed.get; that.title.get :: prev.take(10) }

      li(
        Val(cls:=(if (that.done.get) "task done" else "task")),

        rxCheckbox(that.done),

        rxInput(that.title,
          Val(color:=todoTextColor.get),
          list:="datalist-"+that.hashCode(),
          onchange:=( () => changed.transform(!_) )
          ),

        lastentries.map(it => datalist(
          id:="datalist-"+that.hashCode(),
          it.map(it => option(value:=it)))),

        changeCtr.map(span(_))
      )
    }

}
