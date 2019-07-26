package drx.interface

import java.util.concurrent.ThreadLocalRandom

import drx.Extras
import drx.interface.DSL._

object Fun {
  def mkATodoApp[X](GUI: drx.interface.GUI[X]): X = {
    import GUI._
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

    val rxTask: Task => Blueprint = Extras.lazyExtAttrForPull { that: Task =>
      val changeCtr = Val((that.title.get, that.done.get)).scan(0){ (state, ev) => state + 1 }
      val changed = Var[Boolean](false)
      changed foreach (_ => Todolist.removeEmptyTodos())
      val lastentries = Val((changed.get, that.title.get)).scan(List[String]()){ (state, ev) => ev._2 :: state.take(10) }

      hbox(
        //Val(cls := (if (that.done.get) "task done" else "task")),
        sCheckbox(that.done),
        sInput(that.title,
          Val(color(Todolist.todoTextColor.get))
          //        list("datalist-" + that.hashCode()),
          //scallback(_ => changed.transform(!_))
        ),
        //      lastentries.map(it => datalist(
        //        id := "datalist-" + that.hashCode(),
        //        it.map(it => option(value := it)))),
        changeCtr.map(x=>label(text(x.toString))))
    }

    def main(): Blueprint = {
      val todotext = Todolist.text.map(x=>label(text(x)))
      val textlen = Todolist.len.map(x=>label(text(x.toString)))

      // Val(Todolist.model.aggregate.get.mapValues(rxTask))
      // --> Todolist.model.aggregate.map(_.mapValues(rxTask))
      // --> Todolist.model.diffs.mapmapValues(rxTask)

      val todolist = vbox(
        vbox(Todolist.model.aggregate.map(_.mapValues(rxTask))),
        label(Val(if (Todolist.model.aggregate.get.isEmpty) text("All done! :)") else text(""))))

      vbox(gap(.01),
        vbox(
          label(text("DO TODOS! ")/*, drx.Network.localId*/),
          sCommand(Todolist.addNewTodo, promptText("enter new todo here"))),

        hbox(gap(.01), vbox(todolist), vbox(todolist)),

        flow(
          label(text("There ")), todotext,
          label(text(" left, with a total description length of ")),
          textlen, label(text("."))),

        sButton(Todolist.removeDoneTodos, text("remove all done todos"),
          Todolist.model.aggregate.map(!_.exists(_._2.done.get)).map(disabled(_)))
      )
    }

    main().render
  }
}