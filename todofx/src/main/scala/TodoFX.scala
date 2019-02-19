import java.util.concurrent.ThreadLocalRandom

import Rendering.MutatingValue
import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.control._
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
      medium.observer = sig.mkForeach(replaceLastChild(medium, _))
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

    val todoTextColor = Var("g")
    val z = todoTextColor.map(x => x+1).map {x => println(s"dok $x"); x+1} .scan("") {(acc, x) => println("todoTextColor " + x); acc +","+ x}
    todoTextColor set "b"
    todoTextColor set "r"

    val model = new IncMap[Task]()

//    model.diffs foreach (x => println("hello " + x))
//    model.aggregate foreach (x => println("holla " + x))

    def dview(task: Task): javafx.scene.Node = new HBox(10,
      rxCheckBox(task.done), rxTextField(task.title),
      //, task.folded.map(x => new Label(x.toString)).drender
    )

    val mapped2 = model.aggregate
      .map(it => it.values.count(!_.done.get))
      .map(it => if (it == 0) "no" else "" + it)

    val mapped3 = model.aggregate
      .map(_.values.map { task => task.title.get.length }.sum.toString)

    val textfieldstring = Var("")
    val textfield = rxTextField(textfieldstring, "enter new todos here!")
    textfieldstring filter (x => x!="") foreach (x => atomic {
        val y = Task.mk(x)
        model.update(Seq(ThreadLocalRandom.current().nextInt().toString -> y))
        textfieldstring set ""
    })

    val box = new VBox(10)

    val gcbut = new Button("collect")
    gcbut.setOnAction { _ =>
      platform.platform.gc()
      gcbut.setText(platform.platform.heapSize().toString)
    }

    box.getChildren.addAll(Seq[javafx.scene.Node](
      gcbut,
      new Label("DO TODOS!"),
      rxLabel(Val("There are " + mapped2.get + " todos left, " +
        "with a total description length of " + mapped3.get + ".")),
      textfield,
      Val(
        if (model.aggregate.get.isEmpty)
          new Label("All done! :)")
        else
          new VBox(10, model.aggregate.get.values.map(dview).toList:_*)
      ).drender,

      Val(
        if (model.aggregate.get.isEmpty) new Label("")
        else rxButton(() =>
          model.update(model.aggregate.get.filter(_._2.done.get).map(x => (x._1, null)).toSeq))
      ).drender
    ):_*)

    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new ScrollPane())
    Rendering.replaceLastChild(root, box)
    primaryStage.setTitle("AppTodo")
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

  private def rxButton(func: () => Unit): Button = {
    val but = new Button("remove all done todos")
    but.setOnAction(_ => func())
    but
  }

  private def rxLabel(s: Rx[String]): Label = {
    val label = new Label() with MutatingValue
    label.observer = s mkForeach label.setText //  filter(_ => false)
    label.setWrapText(true)
    label
  }

  private def rxTextField(title: Var[String], placeholder: String = ""): TextField = {
    val field = new TextField() with MutatingValue
    field.observer = title mkForeach field.setText
    field.setPromptText(placeholder)
    field.setOnAction(_ => title.set(field.getText()))
    field
  }

  private def rxCheckBox(done: Var[Boolean]): CheckBox = {
    val checkbox = new CheckBox() with MutatingValue
    checkbox.observer = done mkForeach checkbox.setSelected
    checkbox.setOnAction { _ => done.transform(!_) }
    checkbox
  }
}