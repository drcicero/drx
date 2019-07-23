package standard

import java.util.concurrent.ThreadLocalRandom

import org.scalajs.dom
import scalatags.JsDom
import scalatags.JsDom.all._

import drx.interface.DSL._
import drx.Extras

import SDom._
import SDomHelper._

object SAppTodo {
  drx.interface.DSL.innerdsl = drx.pull.pullDSL

  case class Task(title: Var[String], done: Var[Boolean]) {
    val folded: Val[Int] = title.map(_=>1) // .scan(0)((state, event) => state + 1) // TODO
  }
  object Task {
    def mk(titleStr: String): Task = {
      val title = Var(titleStr)
      val desc = Var(false)
      Task(title, desc)
    }
//    import upickle.default._
//    implicit def rw: ReadWriter[Task] = macroRW
  }

  object Todolist {
    var todoTextColor: Var[String] = Var("green")

    val model: VarMap[Task] = VarMap()

    val text: Val[String] = model.aggregate
      .map(it => it.values.count(!_.done.get))
      .map(it => if (it == 0) "no" else "" + it)

    val len: Val[Int] = model.aggregate
      .map { it => it.toList.map { task => task._2.title.get.length }.sum }

    def removeEmptyTodos(): Unit = model.keep(_.title.sample.isEmpty)
    def removeDoneTodos(): Unit  = model.keep(_.done.sample)
    def addNewTodo(x: String): Unit = {
      val y = Task.mk(x)
      val rnd = ThreadLocalRandom.current().nextLong().toHexString
      model.update(Seq(rnd -> Polarized(true, y)))
    }
  }

  def main(): Unit = {
    val todotext = Todolist.text.map(span(_))
    val textlen = Todolist.len.map(span(_))

    // Val(Todolist.model.aggregate.get.mapValues(rxTask))
    // --> Todolist.model.aggregate.map(_.mapValues(rxTask))
    // --> Todolist.model.diffs.mapmapValues(rxTask)

    val todolist = div(
//      span(Val { span(Todolist.model.aggregate.get.toString) }),
      ul(Todolist.model.aggregate.map(_.mapValues(rxTask))),
      div(Todolist.model.aggregate.map(lst => if (lst.isEmpty) cls:="info" else cls:="hidden"), "All done! :)"))

    val obj = div(
      h1("DO TODOS! "/*, drx.Network.localId*/),
      sCommand(Todolist.addNewTodo, placeholder:="enter new todo here"),

      //      div(todolist),
      div(todolist, style:="display:inline-block; width:48%"),
      div(style:="display:inline-block; width:4%"),
      div(todolist, style:="display:inline-block; width:48%"),
      //      div(style:="display:inline-block; width:2%"),
      //      div(allOtherModels.map(x => span(x._1)), todolist2, style:="display:inline-block; width:48%"),

      p("There ", todotext, " left, " +
        "with a total description length of ", textlen, "."),
      sButton(
        Todolist.removeDoneTodos, value:="remove all done todos",
        Todolist.model.aggregate.map(!_.exists(_._2.done.get)).map(toggleDisplayNone)),
    )
    println("test")
    replaceChild(dom.document.body, dom.document.body.lastElementChild, obj.render)
    println("toast")
    transact()
  }

  val rxTask: Task => JsDom.TypedTag[dom.Element] = Extras.lazyExtAttrForPull { that: Task =>
    val changeCtr = Val((that.title.get, that.done.get)).scan(0){ (state, ev) => state + 1 }
    val changed = Var[Boolean](false)
    changed foreach (_ => Todolist.removeEmptyTodos())
    val lastentries = Val((changed.get, that.title.get)).scan(List[String]()){ (state, ev) => ev._2 :: state.take(10) }

    li(
      Val(cls := (if (that.done.get) "task done" else "task")),
      sCheckbox(that.done),
      sInput(that.title,
        Val(color := Todolist.todoTextColor.get),
        list := "datalist-" + that.hashCode(),
        onchange := (() => changed.transform(!_))),
      lastentries.map(it => datalist(
        id := "datalist-" + that.hashCode(),
        it.map(it => option(value := it)))),
      changeCtr.map(span(_)))
  }
}
