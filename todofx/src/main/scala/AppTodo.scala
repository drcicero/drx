import javafx.application.Application
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.Node
import javafx.stage.Stage
import drx._
import main.{Task, Todolist}
import RxFxHelper._
import drx.graph.{Rx, Var}

class AppTodo extends Application {

  override def start(primaryStage: Stage): Unit = {
    concreteplatform.fpost("test", "test")

    import RxFx.SignalToNode

//    val todoTextColor = Var("g")
//    val z = todoTextColor.map(x => x+1).map {x => println(s"dok $x"); x+1} .scan("") {(acc, x) => println("todoTextColor " + x); acc +","+ x}
//    todoTextColor set "b"
//    todoTextColor set "r"

    def dview(task: Task): Node = new HBox(10,
      rxCheckBox(task.done), rxTextField(task.title),
      task.folded.map(x => label(x.toString)).drender
    )


    val textfieldstring = Var("")
    val textfield = rxTextField(textfieldstring, "enter new todos here!")
    textfieldstring filter (x => x!="") foreach (x => atomic {
      Todolist.addNewTodo(x)
      textfieldstring set ""
    })

    val gcbuttext = Var("collect")
    val gcbut = rxButton(gcbuttext, { () =>
      concreteplatform.gc()
      gcbuttext set concreteplatform.heapSize().toString
    })

    val allOtherModels: Rx[Map[String, Task]] =
       drx.Network.getAllOthers(Todolist.model.diffs, "todos", () => Todolist.model.aggregate.sample.toSeq)
         .filter(x => x._1 != drx.Network.thisClient)
         .scan(Map[String, Task]()){ (state, event) =>
           println((state, event))
           (state ++ event._2) filter { _._2 != null } }
    drx.Network.startHeartbeat()

    val box = new VBox(10)
    box.getChildren.addAll(Seq[Node](
      gcbut,

      label("DO TODOS!"),
      rxLabel(Val("There are " + Todolist.text.get + " todos left, " +
        "with a total description length of " + Todolist.len.get + ".")),
      textfield,
      Val(
        if (Todolist.model.aggregate.get.isEmpty)
          label("All done! :)")
        else
          new VBox(10, Todolist.model.aggregate.get.values.map(dview).toList:_*)
      ).drender,
      Val(
        if (allOtherModels.get.isEmpty)
          label("All done! :)")
        else
          new VBox(10, allOtherModels.get.values.map(dview).toList:_*)
      ).drender,
      Val(
        if (Todolist.model.aggregate.get.isEmpty) label("")
        else rxButton(Val("remove all done todos"), Todolist.removeDoneTodos)).drender
    ):_*)

    startWith(primaryStage, "AppTodo", box)
  }

}