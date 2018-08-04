/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtensionAttr blocks...?

import RxDom._
import drx._

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.DataList

import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import scala.scalajs.js
import scala.language.implicitConversions

//@js.native @js.annotation.JSGlobal("Viz")
//object Viz {}

object TodoJS {

//  val fullName: Variable[String] = new Variable("")

  // todos
  var uniq = 0
  object Task {
    type SerializeType = (String, ((String, String), (String, Boolean)))
    type Ctor = (String, String, Boolean)
    type MapType = (String, (String, Boolean))
    def apply(ind: Ctor) = new Task(ind._1, ind._2, ind._3)
  }
  class Task(val id: String, title_ : String, done_ : Boolean) extends VarOwner {
    val title: Variable[String] = mkVar(title_, "t")
    val done: Variable[Boolean] = mkVar(done_, "d")
    def sampleTuple(): Task.SerializeType = id -> ((title.id, title.sample), (done.id, done.sample))
    override def toString: String = id// title.sample + " " + done.sample
  }
  val model: Store[Task, Task.Ctor] = new Store(Task.apply, "model")
  val model2: Store[Task, Task.Ctor] = new Store(Task.apply, "model")
  val todoTextColor: Variable[String] = new Variable("green", "col")
  def makeNewTodo(value: String): Unit = model.create({uniq+=1; uniq.toString}, value, false)
  def clearDoneTodos(x:Unit): Unit = model.remove(model.sample.filter(it =>
    it.done.sample || it.title.sample.isEmpty))

  val messaging = new Channel[Unit]("SEND")
  val metaing   = new Channel[Unit]("SEND")

  // snake game
//  val clock = new Variable(0)
//  class Cell(val x: Int, val y: Int, c: Int) extends VarOwner {
//    val content: Variable[Int] = mkVar(c, "c")
//    override def toString: String = content.sample.toString
//  }
//  val game = new Store((xyc: (Int, Int, Int)) => new Cell(xyc._1, xyc._2, xyc._3), "game")
//  def tick(): Unit = {}
//  def rxCell(cell: Cell): TypedTag[dom.html.Div] = {
//    div(
//      Signal(style:="position:absolute;left:"+(100*cell.x/10.0)+"%;top:"+(100*cell.y/10.0)+"%;width:9%;height:9%;background:rgba("+(cell.content.get*255.0/3)+",0,0,1)")
//    )
//  }

  def main(args: Array[String]): Unit = {
    val svg_container = dom.document.querySelector("#svg-container")
    debug.hook = {() =>
      val string = drx.debug.stringit(collectChildSinks(dom.document.body))
      println(string)
      val but = button("render").render
      but.onclick = { e =>
        but.outerHTML = dom.window.asInstanceOf[js.Dynamic]
          .Viz(string, Map("engine" -> "dot")).asInstanceOf[String]
      }
      svg_container.appendChild(but)
    }

//    for (y <- 0 to 9; x <- 0 to 9)
//      yield game.create((x, y, if (x==4&&y==4) 1 else if (x==4&&y==3) 2 else 0))

//    model.create({uniq+=1; uniq.toString}, "milk", false)
//    model2.create({uniq+=1; uniq.toString}, "cheese", false)
//    model.create({uniq+=1; uniq.toString}, "choco", false)
//    model.create({uniq+=1; uniq.toString}, "honey", false)

    // networking {
    import scala.collection.mutable.Map
    val meta      = scala.collection.mutable.ListBuffer[(String, Object)]()
    val messages  = Map[(String, String), Object]()

//    metaing observe {
//    }

    val incoming  = Map[String, Map[(String, String), EventSource[_]]]()

    def pub(name: String, minus: Iterable[Task], plus: Iterable[Task]): Unit = {
      val minusid = minus.map(_.id).toSeq
      meta += (name, (minusid, plus.map(_.sampleTuple).toSeq)).asInstanceOf[(String, Object)]
      plus flatMap (_.getEventsources) foreach { vari => vari.changes() observe { valu =>
        val tup = (name, vari.id) -> valu.asInstanceOf[Object]
        messages += tup
        println(s"send $tup")
      } }
    }

    def publish(store: Store[Task, Task.Ctor], localname: String): Unit = {
      pub(localname, None, store.sample)
      store.diffs.observe { case(a,b) => pub(localname, a, b) }
    }

    def subscribe(remote: String, store: Store[Task,Task.Ctor], local: String): Unit = {
      val lincoming = incoming.getOrElse(local, {
        val map = Map[(String, String), EventSource[_]]()
        incoming(local) = map
        map
      })

      messaging observe { Unit =>
        messages filter (kv => kv._1._1 == remote) foreach { case (name, value) =>
          messages -= name
//          val localsource = lincoming(name).asInstanceOf[EventSource[Object]]
////          localsource fire value.asInstanceOf[Object]
//          camein((local, localsource.id)) = value.asInstanceOf[Object]
//          println(s"  recv $name -> $value -> ${localsource.id}")
        }
      }

      metaing observe { Unit =>
        val x = meta.asInstanceOf[Seq[(String, (Seq[String], Seq[Task.SerializeType]))]]
          .filter(y => y._1 == remote)
        meta --= x
        if (x.size>0) {

  //        println("local  " + local)
  //        println("red " + x)

          val sample   = store.sample
          val minus    = x.flatMap(_._2._1).toSet
          val plus     = x.flatMap(_._2._2).toMap

          val addition = plus -- sample.map(_.id)
          val update   = plus -- addition.map(_._1)
          val subtract = sample filter (i => minus contains i.id)
          val update2  = sample filter (i => plus  contains i.id)

  //        println("plus " + plus)
  //        println("sub  " + subtract)

          println("add " + addition)
          val vars  = store.update(addition.map(it => (it._1, it._2._1._2, it._2._2._2)), subtract)
          val left  = addition.flatMap(it => Seq(it._2._1._1, it._2._2._1))
          val right = vars.flatMap(it => it.getEventsources)
          lincoming --= subtract.flatMap(_.getEventsources.map(it => (remote, it.id)))
          lincoming ++= left.map(it => (remote, it)) zip right
          lincoming ++=(update.toSeq.flatMap(it => Seq((remote, it._2._1._1), (remote, it._2._2._1)))
                        zip update2.flatMap(_.getEventsources))

          update foreach { it =>
            sample.find(_.id == it._1).get.title set it._2._1._2
            sample.find(_.id == it._1).get.done  set it._2._2._2
          }

          println("in "+ local +" "+ lincoming.map { case(a,b) => a -> b.id })
        }
      }
    }
    // networking }

    publish(model,"left")
    subscribe("left",     model2,"right")

    publish(model2,"right")
    subscribe("right",    model,"left")
    metaing send (())

    def func():Unit = scala.scalajs.js.timers.setTimeout(1000) {
//      clock.set(scalajs.js.Date.now.toInt)
      if (meta.size + messages.size > 0) println("tick")
      while (meta.size > 0)     metaing.send(())
      while (messages.size > 0) messaging.send(())
      func()
    }
    func()

    val todotext = model
      .map({ it => it.count(!_.done.get) }, "notdone")
      .map({ it => span(if (it == 0) "are no active todos"
                        else if (it == 1) "is 1 active todo"
                        else "are " + it + " active todos") }, "string")
    val textlen = model
      .map({ it => span(it.toList.map { task => task.title.get.length }.sum) }, "charsum")
    // val todolistlist = model.map { lst => ul(lst.toSeq.map(rxTask(model))) }
    val todolist2 = ul(model2.dmapmap(rxTask(model2)))
    val todolistlist = ul(model.dmapmap(rxTask(model)))
    val todolist = model map ( lst =>
      if (lst.isEmpty) div(cls:="info", "All done! :)")
      else todolistlist, "tasklist")

//    val gamefield = div(style:="position:relative;height:320px", game dmapmap rxCell)

    val log = textarea(id:="log").render

    val obj = div(
//      rxClock(), br,
//      rxFullName(Signal("So be it!"), fullName), br,
//      "Hello ", fullName.map(span(_)), "!", br,

      h1("DO TODOS!"),
      rxCommand(makeNewTodo, placeholder:="enter new todo here"),

      div(todolist, style:="display:inline-block; width:32%"),
      div(style:="display:inline-block; width:2%"),
      div(todolist, style:="display:inline-block; width:32%"),
      div(style:="display:inline-block; width:2%"),
      div(todolist2, style:="display:inline-block; width:32%"),

      p("There ", todotext, " left, " +
        "with a total description length of ", textlen, "."),
      rxButton(
        Signal("remove all done todos"), clearDoneTodos,
        model.map(_.filter(_.done.get).isEmpty).map(toggleDisplayNone)),

      br, br, br,

//      gamefield,

      p("debug:"),
      button("gen ten", onclick:=(() =>
        model.creates( for (i <- 0 to 10) yield ({uniq+=1; uniq.toString}, "unique" + i, false) ))),
      button("del ten", onclick:=(() =>
        model.remove(model.sample))),

      button("paint", onclick:=(() =>
        log.value = drx.debug.stringit(collectChildSinks(dom.document.body)))),

      log,

      button(
        style:="display:block", onclick:={ () =>
          val tmp = dom.document.querySelector("#svg-container")
          tmp.innerHTML += dom.window.asInstanceOf[js.Dynamic]
            .Viz(drx.debug.stringit(collectChildSinks(dom.document.body)),
                 Map("engine" -> "dot")).asInstanceOf[String]
        }, "So be it!"),

//      button("doit", onclick:={ () => drx.helper.printless() }),
    )
    transact { insertChild(dom.document.body, obj.render) }
  }

  val rxTask: (Store[Task, _]) => (Task) => TypedTag[dom.html.Element] = (store) => Extras.lazyExtAttr { that =>
    val changeCtr = Extras.zip(that.title, that.done).changes().fold(0)((state, event) => state + 1)

    val changed = new Channel[Unit]()
    changed observe (_ => store.remove(store.sample.filter(it => it.title.sample.isEmpty)))
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
      rxButton(Signal("submit"), clicked.send,
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

  def rxCommand(sig: String => Unit, m:Modifier*) = input(
    tpe:="text",
    onchange:={ e: dom.Event =>
      val inputElem = e.target.asInstanceOf[dom.html.Input]
      sig(inputElem.value)
      inputElem.value = ""
    },
    m)

  def rxButton(sig: Rx[String], click: Unit => Unit, m:Modifier*) = input(
    tpe:="button",
    Signal(value:=sig.get),
    onclick:=( () => click(()) ),
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
