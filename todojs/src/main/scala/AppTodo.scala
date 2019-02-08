/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

import java.util.concurrent.ThreadLocalRandom

import Network.{ClientID, fetchBase64, fetchText}
import RxDom._
import RxDomHelper._
import drx._
import org.scalajs.dom
import scalatags.JsDom
import scalatags.JsDom.all._
import upickle.default.{ReadWriter, macroRW}

import scala.language.implicitConversions

object AppTodo {

  val fullName: Var[String] = new Var("", "fullname")

  case class Task(id: String, title: Var[String], done: Var[Boolean])
  object Task { implicit def rw: ReadWriter[Task] = macroRW }

  val model  = new IncMap[Task]("model")
  model.aggregate

  //  val model2 = new Store((Task.apply _).tupled, "model")
  val todoTextColor: Var[String] = new Var("green", "col")
  def makeNewTodo(value: String): Unit = model.update {
    val rnd = ThreadLocalRandom.current().nextInt().toString
    Seq(rnd -> Task(rnd, new Var(value), new Var(false)))
  }
  def clearDoneTodos(store: IncMap[Task]): Unit = {
    println("clearing!")
    store.update(store.aggregate.sample.toSeq
      filter (_._2.title.sample.isEmpty)
      map (kv => (kv._1, null)))
  }

  // snake game
//  val clock = new Var(0)
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

  def main(): Unit = {
    val ximg = new Var("")

    val svg_container = dom.document.querySelector("#svg-container")
//    debug.hook = {str =>
//      val string = drx.debug.stringit(collectChildSinks(dom.document.body))
////      println(string)
//      val but = button("render").render
//      but.onclick = { e =>
//        but.outerHTML = dom.window.asInstanceOf[js.Dynamic]
//          .Viz(string, Map("engine" -> "dot")).asInstanceOf[String]
//      }
//      svg_container.appendChild(span(str).render)
//      svg_container.appendChild(but)
//      svg_container.appendChild(br.render)
//    }

//    for (y <- 0 to 9; x <- 0 to 9)
//      yield game.create((x, y, if (x==4&&y==4) 1 else if (x==4&&y==3) 2 else 0))

//    transact {
//      model.create((ThreadLocalRandom.current().nextInt().toString, "milk", false))
//      model2.create((ThreadLocalRandom.current().nextInt().toString, "cheese", false))
//      model.create((ThreadLocalRandom.current().nextInt().toString, "choco", false))
//      model.create((ThreadLocalRandom.current().nextInt().toString, "honey", false))
//    }

    Network.pub(model.diffs)
    val allOtherModels: Rx[(ClientID, Map[String, Task])] =
      Network.sub[Map[String, Task]](Map(), model.diffs.id)
        .scan(("", Map[String, Task]())){ (state, event) => (event._1, (state._2 ++ event._2) filter { _._2 != null }) }

    //Network.publish(fullName)
    //Network.subscribe[String]("", "fullname") observe {
    //  case (_, valu) => fullName set valu
    //}

    Network.startHeartbeat()

    val todotext = model.aggregate.map({ lst =>
      val it = lst.size
      span(if (it == 0) "are no active todos"
           else if (it == 1) "is 1 active todo"
           else "are " + it + " active todos") }, "string")
    val textlen = model.aggregate
      .map({ it => span(it.toList.map { case (k,task) => task.title.get.length }.sum) }, "charsum")
//    val todolistlist = model.map { lst => ul(lst.toSeq.map(rxTask(model))) }
    val todolistlist = ul(model.diffs.dmapmap(rxTask(() => clearDoneTodos(model))))
    val todolist = model.aggregate map (lst =>
      if (lst.isEmpty) div(cls:="info", "All done! :)")
      else todolistlist, "tasklist")
    val todolistlist2 = ul(allOtherModels.map(x => span(x._2.map(x => rxTask(() => ())(x._2)).toSeq)))
    val todolist2 = allOtherModels map (lst =>
      if (lst._2.isEmpty) div(cls:="info", "All done! :)")
      else todolistlist2, "tasklist")

//    val gamefield = div(style:="position:relative;height:320px", game dmapmap rxCell)

//    val log = textarea(id:="log").render

    val obj = div(
//      rxClock(), br,
//      rxFullName(Signal("So be it!"), fullName), br,
//      "Hello ", fullName.map(span(_)), "!", br,

      h1("DO TODOS! ", Network.name.map(x => span(x))),
      rxCommand(makeNewTodo, placeholder:="enter new todo here"),

//      div(todolist),
      div(Network.name.map(span(_)), todolist, style:="display:inline-block; width:48%"),
      div(style:="display:inline-block; width:4%"),
//      div(todolist, style:="display:inline-block; width:48%"),
//      div(style:="display:inline-block; width:2%"),
      div(allOtherModels.map(x => span(x._1)), todolist2, style:="display:inline-block; width:48%"),

      p("There ", todotext, " left, " +
        "with a total description length of ", textlen, "."),
      rxButton(
        () => clearDoneTodos(model), value:="remove all done todos",
        model.aggregate.map(!_.exists(_._2.done.get)).map(toggleDisplayNone)),

      br, br, br,

      ximg.map(x => img(src:=x))

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
//          tmp.innerHTML += dom.window.asInstanceOf[js.Dynamic]
//            .Viz(drx.debug.stringit(collectChildSinks(dom.document.body)),
//                 Map("engine" -> "dot")).asInstanceOf[String]
//        }, "So be it!"),

//      button("doit", onclick:={ () => drx.helper.printless() }),
    )
//    transact { // TODO what for the transact?
    replaceChild(dom.document.body, dom.document.body.lastElementChild, obj.render)
//    }

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    val dino = "http://dinoipsum.herokuapp.com/api/?format=text&paragraphs=3"

    fetchText(dino)
    .flatMap(_ => fetchText(dino))
    .flatMap(_ => fetchText(dino))
    .flatMap(_ => fetchText("http://www.random.org/integers/" +
      "?num=1&min=10&max=55&base=10&format=plain&rnd=new&col=1"))
    .flatMap{ x =>
      println((2, x.trim.toInt));
      val a = fetchBase64("https://picsum.photos/"+(x.trim.toInt * 10)+"/200/")
      val b = fetchText(dino)
      Future.sequence(Seq(a, b)) }
    .foreach(x => ximg set "data:image/jpeg;base64," + x(0))
  }

  val rxTask: (() => Unit) => Task => JsDom.TypedTag[dom.html.Element] =
    onclick => Extras.lazyExtAttr { that =>
      val changeCtr = Scan(0){ prev => that.title.get; that.done.get; prev + 1 }

      val changed = new Var[Boolean](false)
      changed observe (_ => onclick())
      val lastentries = Scan(List[String]()){ prev =>
        changed.get; that.title.get :: prev.take(10) }

      li(
  //      cls:="task",
        Val(cls:=(if (that.done.get) "task done" else "task")),

  //      span,
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
