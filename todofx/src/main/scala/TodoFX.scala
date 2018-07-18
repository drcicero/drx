//import javafx.collections.ObservableList
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javafx.application.{Platform, Application}
import javafx.geometry.Insets
import javafx.scene.control.{Button, CheckBox, Label, TextField}
import javafx.scene.layout.{HBox, Pane, StackPane, VBox}
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage

import drx._
import main.Task

import scala.collection.mutable

object Rendering {
  trait MutatingValue extends Node { var observer: Sink[_] = _ }

  def replaceLastChild(medium: Pane, newelem: Node): Unit = {
    foreachObs(medium)(_.stop())
    medium.getChildren.remove(0)
    medium.getChildren.add(newelem)
    if (medium.isVisible) foreachObs(medium)(_.start())
  }

  def foreachObs(fc: Parent)(f: Sink[_] => Unit): Unit = {
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

class TodoFX extends Application {
  import Rendering.SignalToNode

  override def start(primaryStage: Stage): Unit = transact {
    val todoTextColor: Variable[String] = new Variable("green", "col")
    val model: Store[Task, String] = new Store((name: String) => new Task(name), "model")

    def dview(task: Task): javafx.scene.Node = new HBox(10, Signal {
      val checkbox = new CheckBox()
      checkbox.setSelected(task.done.get)
      checkbox.setOnAction(_ => task.done.transform(!_))
      checkbox
    }.drender, Signal {
      val field = new TextField(task.title.get)
      field.setOnAction(_ => task.title.set(field.getText()))
      field
    }.drender
    //, task.folded.map(x => new Label(x.toString)).drender
    )

    val mapped2 = model
      .map({ it => it.count(!_.done.get) }, "notdone")
      .map({ it => if (it == 0) "no" else ""+it }, "string")

    val mapped3 = model
      .map(_.map { task => task.title.get.length }.sum.toString, "charsum")

    val textfield = new TextField("enter new task here")
    textfield.setOnAction { _ =>
      model.create(textfield.getText)
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

      Signal({
        val label = new Label("There are " + mapped2.get + " todos left, " +
          "with a total description length of " + mapped3.get + ".")
        label.setWrapText(true)
        label
      }, "desc").drender,

      textfield,

      Signal(
        if (model.get.isEmpty)
          new Label("All done! :)")
        else
          new VBox(10, model.get.map(dview).toList:_*)
        , "tasklist").drender,

      Signal(
        if (model.get.isEmpty)
          new Label("")
        else {
          val but = new Button("remove all done todos")
          but.setOnAction(_ =>
            model.remove(model.sample.filter(_.done.sample)))
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

  // launch must be inside our Application, because Application.launch
  // checks that enclosing class is subclass of Application...
  private[TodoFX] def launchRedirect(args: Array[String]): Unit = Application.launch(args:_*)
}

object TodoFX {
  def main(args: Array[String]): Unit = new TodoFX().launchRedirect(args)
}
