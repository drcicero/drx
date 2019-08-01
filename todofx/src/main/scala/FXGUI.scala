package away

import scala.collection.JavaConverters._
import drx.interface.GUI
import javafx.application.Application
import javafx.scene.layout.{FlowPane, HBox, Pane, VBox}
import javafx.scene.{Node, Parent}
import javafx.stage.Stage
import drx._
import main.{Task, Todolist}
import drx.graph.{Rx, Val, Var}
import javafx.scene.control.{Button, CheckBox, Label, TextField}

import scala.collection.mutable

object FXGUI extends GUI[Node] {
  override def isRooted(w: Node): Boolean = true
  override def getParent(w: Node): Node = w.getParent
  override def getChildren(w: Node): Seq[Node] = w.asInstanceOf[Parent].getChildrenUnmodifiable.asScala
  override def appendRaw(parent: Node, child: Node): Unit = parent.asInstanceOf[Pane].getChildren.add(child)
  override def replaceRaw(old: Node, next: Node): Unit = {
    val c = old.getParent.asInstanceOf[Pane].getChildren
    c.add(c.indexOf(old), next)
    c.remove(old)
  }
  override def removeRaw(w: Node): Unit = w.getParent.asInstanceOf[Pane].getChildren.remove(w)

  override def textOf(w: Node): String = w match { case w: TextField => w.getText() }

  def style(s: String): Mod = w => () // w.setAttribute("style", w.getAttribute("style") + "; " + s)
  override def gap(i: Double): Mod = w => () //
  override def color(c: String): Mod = w => () //
  override def width(i: Double): Mod = w => w.maxWidth(i*100)
  override def height(h: Double): Mod = w => w.maxHeight(i*100)
  override def callback(f: Node => Unit): Mod = {
    case w: TextField => w.setOnAction(_ => f(w))
    case w: CheckBox => w.setOnAction(_ => f(w))
    case w: Button => w.setOnAction(_ => f(w))
    case w => println(w, "callback error")
  }
  override def disabled(b: Boolean): Mod = {
    case w: TextField => w.setDisable(b)
    case w: CheckBox => w.setDisable(b)
    case w: Button => w.setDisable(b)
  }
  override def text(f: String): Mod = {
    case w: Button => w.setText(f)
    case w: TextField => w.setText(f)
    case w: Label => w.setText(f)
    case w => println(w, "text error")
  }
  override def promptText(f: String): Mod = { case w: TextField => w.setPromptText(f) }
  override def checked(f: Boolean): Mod = { case w: CheckBox => w.setSelected(f) }

  var i = 0
  private def createElement(gen: => Node, ms: Mod*): Blueprint = new Blueprint {
    def render: Node = {
      val d = gen
      ms foreach(_.applyTo(d))
      d
    }
  }
  override def button(ms: Mod*): Blueprint = createElement(new Button(), ms:_*)
  override def vbox(ms: Mod*): Blueprint = createElement(new VBox(), ms:_*)
  override def hbox(ms: Mod*): Blueprint = createElement(new HBox(), ms:_*)
  override def flow(ms: Mod*): Blueprint = createElement(new FlowPane(), ms:_*)
  override def label(ms: Mod*): Blueprint = createElement(new Label(), ms:_*)
  override def input(ms: Mod*): Blueprint = createElement(new TextField(), ms:_*)
  override def checkbox(ms: Mod*): Blueprint = createElement(new CheckBox(), ms :_*)

  private val DATA_IS_REACTIVE = "data-is-reactive"
  private val remember = concreteplatform.WeakMap[Node, Boolean]()

  def getMarkedChildren(parent: Node): Seq[Node] = {
    val lst = mutable.Buffer[Node]()
    parent match {
      case parent: Parent =>
        parent.getChildrenUnmodifiable.forEach { child =>
          if (remember.get(child).isDefined) lst.append(child)
          lst.appendAll(getMarkedChildren(child))
        }
      case _ =>
    }
    lst
  }
  override def mark(it: Node): Unit = remember.set(it, true)
  override def unmark(it: Node): Unit = remember.set(it, false)
}
