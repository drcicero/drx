import RxFx.MutatingValue
import drx.graph.{Obs, Rx, Var}
import javafx.geometry.Insets
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control.{Button, CheckBox, Label, ScrollPane, TextField}
import javafx.scene.layout.{Pane, StackPane}
import javafx.stage.Stage

object RxFx {
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

  implicit class SignalToNode(val sig: Rx[Node]) extends AnyVal {
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

object RxFxHelper {
  def startWith(primaryStage: Stage, title: String, box: javafx.scene.Node): Unit = {
    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new ScrollPane())
    RxFx.replaceLastChild(root, box)
    primaryStage.setTitle(title)
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

  def label(title: String): Label = new Label(title)

  def rxButton(title: Rx[String], func: () => Unit): Button = {
    val but = new Button() with MutatingValue
    but.observer = title mkForeach but.setText
    but.setOnAction(_ => func())
    but
  }

  def rxLabel(s: Rx[String]): Label = {
    val label = new Label() with MutatingValue
    label.observer = s mkForeach label.setText //  filter(_ => false)
    label.setWrapText(true)
    label
  }

  def rxTextField(title: Var[String], placeholder: String = ""): TextField = {
    val field = new TextField() with MutatingValue
    field.observer = title mkForeach field.setText
    field.setPromptText(placeholder)
    field.setOnAction(_ => title.set(field.getText()))
    field
  }

  def rxCheckBox(done: Var[Boolean]): CheckBox = {
    val checkbox = new CheckBox() with MutatingValue
    checkbox.observer = done mkForeach checkbox.setSelected
    checkbox.setOnAction { _ => done.transform(!_) }
    checkbox
  }}
