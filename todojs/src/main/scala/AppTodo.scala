/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

import drx.graph.{Scan, Val, Var}
import main.{Task, Todolist}

import scala.collection.mutable
import scala.scalajs.js

//import drx.Network._
import RxDom._
import RxDomHelper._
import drx._
import org.scalajs.dom
import scalatags.JsDom
import scalatags.JsDom.all._

//import scala.language.implicitConversions

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

//      rxClock(), br,



//val fullName: Var[String] = Var("")
//drx.Network.publish(fullName)
//drx.Network.subscribe[String]("", "fullname") observe {
//  case (_, valu) => fullName set valu
//}
//      rxFullName(Signal("So be it!"), fullName), br,
//      "Hello ", fullName.map(span(_)), "!", br,



//br, br, br,
// snake game
//  val clock = Var(0)
//  class Cell(val x: Int, val y: Int, c: Int) extends VarOwner {
//    val content: Var[Int] = mkVar(c, "c")
//    override def toString: String = content.sample.toString
//  }
//  val game = new Store((xyc: (Int, Int, Int)) => new Cell(xyc._1, xyc._2, xyc._3), "game")
//  def tick(): Unit = {}
//  def rxCell(cell: Cell): JsDom.TypedTag[dom.html.Div] = {
//    div(
//      Signal(style:="position:absolute;left:"+(100*cell.x/10.0)+"%;top:"+(100*cell.y/10.0)+"%;width:9%;height:9%;background:rgba("+(cell.content.get*255.0/3)+",0,0,1)")
//    )
//  }
//    val gamefield = div(style:="position:relative;height:320px", game dmapmap rxCell)

//    for (y <- 0 to 9; x <- 0 to 9)
//      yield game.create((x, y, if (x==4&&y==4) 1 else if (x==4&&y==3) 2 else 0))

//      gamefield,

//      p("debug:"),
//      button("gen ten", onclick:=(() =>
//        model.creates( for (i <- 0 to 10) yield ({uniq+=1; uniq.toString}, "unique" + i, false) ))),
//      button("del ten", onclick:=(() =>
//        model.remove(model.sample))),

//      button("paint", onclick:=(() =>
//        log.value = drx.debug.stringit(collectChildSinks(dom.document.body)))),

//      log,

//      button(
//        style:="display:block", onclick:={ () =>
//          val tmp = dom.document.querySelector("#svg-container")
//          tmp.innerHTML = tmp.innerHTML + dom.window.asInstanceOf[js.Dynamic]
//            .Viz(drx.debug.stringit(collectChildSinks(dom.document.body)),
//                 Map("engine" -> "dot")).asInstanceOf[String]
//        }, "So be it!"),

//      button("doit", onclick:={ () => drx.helper.printless() }),









//    val ximg = Var("")
//      ximg.map(x => img(src:=x)),
//    import scala.concurrent.ExecutionContext.Implicits.global
//    import scala.concurrent.Future
//    val dino = "http://dinoipsum.herokuapp.com/api/?format=text&paragraphs=3"
//    fetchText(dino)
//    .flatMap(_ => fetchText(dino))
//    .flatMap(_ => fetchText(dino))
//    .flatMap(_ => fetchText("http://www.random.org/integers/" +
//      "?num=1&min=10&max=55&base=10&format=plain&rnd=new&col=1"))
//    .flatMap{ x =>
//      println((2, x.trim.toInt));
//      val a = fetchBase64("https://picsum.photos/"+(x.trim.toInt * 10)+"/200/")
//      val b = fetchText(dino)
//      Future.sequence(Seq(a, b)) }
//    .foreach(x => ximg set "data:image/jpeg;base64," + x(0))
