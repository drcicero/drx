package drx.interface

import drx.interface.DSL._
import drx.interface.Table.{getBag, extend}

object ATodoApp {
  def mkATodoApp[X](GUI: drx.interface.GUI[X]): X = {
    drx.interface.DSL.innerdsl = drx.pull.pullDSL
    import GUI._

    // vars
    final class Todo(_title: String, _done: Boolean) extends Table[Todo] {
      val title: Var[String] = Vary(_title)
      val done: Var[Boolean] = Vary(_done)
    }
    val todoTextColor: Var[String] = Var("green")

    // events
    def removeEmptyTodos(): Unit = Table.getRaw[Todo].filterInPlace(_.title.sample.isEmpty)
    def removeDoneTodos(): Unit = Table.getRaw[Todo].filterInPlace(_.done.sample)
    def addNewTodo(x: String): Unit = transact { new Todo(x, false) } // TODO how to ensure new is transacted?

    // derivatives
    val todotext: Val[String] = getBag[Todo]
      .map{it => println(it.map(_.done)); it.count(!_.done.get)}
      .map(it => if (it == 0) "no" else "" + it)
    val todolen: Val[Int] = getBag[Todo]
      .map(_.map { task => task.title.get.length }.sum)

    // initial state
    transact {
      addNewTodo("helo")
      addNewTodo("fsih")
      addNewTodo("covfefe")
    }

    def doit(x: Int): Unit = {
      val y = x % 10
      if (y < 7) addNewTodo(x.toString)
      else if (y == 8) getBag[Todo].sample.foreach{ x => x.done.set(true) }
      else if (y == 9) removeDoneTodos()
      if (x < 35) drx.concreteplatform.after(100) { () => doit(x+1) }
    }
    drx.concreteplatform.after(2000) { () => doit(0) }

    // extension
    val uiTask = extend { implicit that: Todo =>
      // TODO Val( ... that ... ) produce memory leaks? (but equal map expression does not!)
      val zip         = that.title.zip(that.done)
      val changeCtr   = zip.scan(0){ (state, ev) => state + 1 }
      val lastentries = that.title.scan(List[String]()){ (state, ev) => ev :: state.take(2) }
      that.title foreach (_ => removeEmptyTodos())

      hbox(
        //Val(cls := (if (that.done.get) "task done" else "task")),
        checkbox(checkedBi(that.done)),
        vbox(
          input(textBi(that.title),
            todoTextColor.map(color(_))
            //list("datalist-" + that.hashCode()),
            //callback(_ => changed.transform(!_))
          ),
          vbox(lastentries.map(_.map(x => label(text(x)))))
        ),
        //lastentries.map(it => datalist(
        //  id := "datalist-" + that.hashCode(),
        //  it.map(it => option(value := it)))),

        label(changeCtr.map(x => text(x.toString)))
      )
    }

    // Val(Todolist.model.aggregate.get.mapValues(uiTask))
    // --> Todolist.model.aggregate.map(_.mapValues(uiTask))
    // --> Todolist.model.diffs.mapmapValues(uiTask)

    // gui
    def main(): Blueprint = {
      val uitxt: Blueprint = label(todotext.map(text(_)))
      val uilen: Blueprint = label(todolen.map(x => text(x.toString)))
      val uilist: Val[Seq[Blueprint]] = getBag[Todo].map { lst =>
        if (lst.isEmpty) Seq(label(text("All done! :)")))
        else lst.map(uiTask).toSeq }
      val uiwrap = vbox(gap(10), uilist)

      vbox(gap(10),
        label(text("DO TODOS! ")),
        input(enterTextClear(addNewTodo), promptText("enter new todo here")),

        hbox(gap(10), uiwrap, uiwrap),

        flow(label(text("There are ")), uitxt,
          label(text(" todos left, with a letter total of ")), uilen,
          label(text("."))),

        button(callback(_ => removeDoneTodos()), text("remove all done todos"),
          getBag[Todo].map(!_.exists(_.done.get)).map(disabled(_))))
    }

    main().render
  }
}