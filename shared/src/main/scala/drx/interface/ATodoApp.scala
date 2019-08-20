package drx.interface

import drx.interface.DSL._
import drx.interface.Table.{getData, extend, getDelta}

object ATodoApp {
  def mkATodoApp[X](GUI: drx.interface.GUI[X]): X = {
    drx.interface.DSL.innerdsl = drx.pull.pullDSL
    import GUI._

    // vars
    final class Todo(t: String, d: Boolean) extends Table[Todo] {
      val title: Var[String] = Vary(t)
      val done: Var[Boolean] = Vary(d)
      override def toString: String = "Todo(%s, %s, %b)".format(id, title.sample, done.sample)
    }
    val todoTextColor: Var[String] = Var("green")

    // events
    def removeEmptyTodos(): Unit = Table.getRaw[Todo].filterInPlace(!_.title.sample.isEmpty)
    def removeDoneTodos(): Unit = Table.getRaw[Todo].filterInPlace(!_.done.sample)
    def addNewTodo(x: String): Unit = atomic { new Todo(x, false) } // TODO how to ensure new is transacted?

    import Bag._
    import VarMap.mapmapOps

    // flatmap must store all results per item as Val, and depend on it, if it changes, result is reduce by old and added by new.
    // derivatives
    val todotext: Val[String] = getDelta[Todo]
      .mapFilter { it => !it.done.get }(Table.getRaw[Todo]).mkAggregate(owner = Table.getRaw[Todo], debug = true)
      .map(it => if (it.size == 0) "no" else "" + it.size)
    val todolen: Val[Int] = getData[Todo]
      .map(x => traversableBag(x).map { task => task.title.get.length }.sum)

    // initial state
    atomic {
      addNewTodo("helo")
      addNewTodo("fsih")
      addNewTodo("covfefe")
    }

    def doit(x: Int): Unit = {
      val y = x % 10
      if (y < 7) addNewTodo(x.toString)
      else if (y == 8) getData[Todo].sample.foreach{ x => x.done.set(true) }
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

    import Bag._

    // gui
    val uitxt: Blueprint = label(todotext.map(text(_)))
    val uilen: Blueprint = label(todolen.map(x => text(x.toString)))
    //    val uilist: Blueprint = hbox(
    //      vbox(gap(10), getData[Todo].map(x => traversableBag(x.map(uiTodo)))),
    //      label(getData[Todo].map(x => text(if (x.isEmpty) "All done! :)" else ""))))
    //    val uilist: Val[Seq[Blueprint]] = getBag[Todo].map { lst =>
    //      if (lst.isEmpty) Seq(label(text("All done! :)")))
    //      else lst.map(uiTask).toSeq }
    //    val uiwrap = vbox(gap(10), uilist)

    val main: Blueprint = vbox(gap(10),
      label(text("DO TODOS! ")),
      input(enterTextClear(addNewTodo), promptText("enter new todo here")),
      //        hbox(gap(10), uiwrap, uiwrap),
      flow(label(text("There are ")), uitxt,
        label(text(" todos left, with a letter total of ")), uilen,
        label(text("."))),
      button(callback(_ => removeDoneTodos()), text("remove all done todos"),
        getData[Todo].map(x => !traversableBag(x).exists(_.done.get)).map(disabled(_))))

    main.render
  }
}