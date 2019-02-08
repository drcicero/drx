//import javafx.collections.ObservableList
import java.util.concurrent.ThreadLocalRandom

import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.control.{Button, CheckBox, Label, TextField}
import javafx.scene.layout.{HBox, Pane, StackPane, VBox}
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage
import drx._
import main.Task

object Rendering {
  trait MutatingValue extends Node { var observer: Obs[_] = _ }

  def replaceLastChild(medium: Pane, newelem: Node): Unit = {
    foreachObs(medium)(_.stop())
    medium.getChildren.remove(0)
    medium.getChildren.add(newelem)
    if (medium.isVisible) foreachObs(medium)(_.start())
  }

  def foreachObs(fc: Parent)(f: Obs[_] => Unit): Unit = {
    fc.getChildrenUnmodifiable.forEach { it =>
      it match {
        case x: MutatingValue => f(x.observer)
        case _                =>
      }
      it match {
        case x: Parent => foreachObs(x)(f)
        case _              =>
      }
    }
  }

  implicit class SignalToNode(val sig: Rx[_ <: Node]) extends AnyVal {
    def drender: Node = {
      val medium = new Pane(new Label("{{init}}")) with MutatingValue
      medium.observer = sig.mkObs(replaceLastChild(medium, _))
      medium
    }
  }

//  def collectAllObs(fc: Parent): Set[Sink[_]] = {
//    val set = mutable.Set[Sink[_]]()
//    foreachObs(fc)(set += _)
//    set.toSet[Sink[_]]
//  }
}

object Main {
  def main(args: Array[String]): Unit =
    Application.launch(classOf[TodoFX], args:_*)
}

class TodoFX extends Application {
  override def start(primaryStage: Stage): Unit = {
    import Rendering.SignalToNode

    val todoTextColor = new Var("green", "col")
    todoTextColor.map(_+1).map(_+1).scan("") {(acc, x) => println("todoTextColor " + x); acc +","+ x}
    todoTextColor set "blue"
    todoTextColor set "red"

    val model = new IncMap[Task]("model")
    model.aggregate

    model.diffs observe (x => println("hello " + x))
    model.aggregate observe (x => println("holla " + x))

    def dview(task: Task): javafx.scene.Node = new HBox(10,
      Val {
        val checkbox = new CheckBox()
        checkbox.setSelected(task.done.get)
        checkbox.setOnAction {_ =>
          println("hello")
          task.done.transform(!_)
          println("cello")
        }
        checkbox
      }.drender,
      Val {
        val field = new TextField(task.title.get)
        field.setOnAction(_ => task.title.set(field.getText()))
        field
      }.drender
      //, task.folded.map(x => new Label(x.toString)).drender
    )

    val mapped2 = model.aggregate
      .map({ it => it.values.count(!_.done.get) }, "notdone")
      .map({ it => if (it == 0) "no" else ""+it }, "string")

    val mapped3 = model.aggregate
      .map(_.values.map { task => task.title.get.length }.sum.toString, "charsum")

    val textfield = new TextField("enter new task here")
    textfield.setOnAction { _ =>
      val x = Task.mk(textfield.getText)
      model.update(Seq(ThreadLocalRandom.current().nextInt().toString -> x))
      textfield.setText("")
    }

    val box = new VBox(10)

    val logbut = new Button("log it")
//    logbut.setOnAction(_ => compat2.compat2.writeToDisk())

    val gcbut = new Button("collect")
    gcbut.setOnAction { _ => platform.platform.gc(); gcbut.setText(platform.platform.heapSize().toString) }

    box.getChildren.addAll(Seq[javafx.scene.Node](
      new HBox(10, gcbut, logbut),

      new Label("DO TODOS!"),

      Val({
        val label = new Label("There are " + mapped2.get + " todos left, " +
          "with a total description length of " + mapped3.get + ".")
        label.setWrapText(true)
        label
      }, "desc").drender,

      textfield,

      Val(
        if (model.aggregate.get.isEmpty)
          new Label("All done! :)")
        else
          new VBox(10, model.aggregate.get.values.map(dview).toList:_*)
        , "tasklist").drender,

      Val(
        if (model.aggregate.get.isEmpty)
          new Label("")
        else {
          val but = new Button("remove all done todos")
          but.setOnAction(_ =>
            model.update(model.aggregate.sample.filter(_._2.done.sample).map(x => (x._1, null)).toSeq))
          but
        }, "button").drender
    ):_*)


    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new Pane())
    Rendering.replaceLastChild(root, box)
    primaryStage.setTitle("AppTodo")
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }
}