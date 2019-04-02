/** Created by david on 10.06.17. */
package main

import java.util.concurrent.ThreadLocalRandom

import drx.graph.{Rx, Var}
import drx.IncMap

case class Task(title: Var[String], done: Var[Boolean]) {
  val folded: Rx[Int] = title.map(_+1).scan(0)((state, event) => state + 1)
}

object Task {
  def mk(titleStr: String): Task = {
    val title = Var(titleStr)
    val desc = Var(false)
    Task(title, desc)
  }
  import upickle.default._
  import drx.Network.varRW
  implicit def rw: ReadWriter[Task] = macroRW
}

object Todolist {
  val model: IncMap[Task] = new IncMap[Task]()

  val text: Rx[String] = model.aggregate
    .map(it => it.values.count(!_.done.get))
    .map(it => if (it == 0) "no" else "" + it)

  val len: Rx[Int] = model.aggregate
    .map { it => it.toList.map { task => task._2.title.get.length }.sum }

  def removeEmptyTodos(): Unit = model.remove(_.title.sample.nonEmpty)
  def removeDoneTodos(): Unit  = model.remove(!_.done.sample)
  def addNewTodo(x: String): Unit = {
    val y = Task.mk(x)
    val rnd = ThreadLocalRandom.current().nextLong().toHexString
    model.update(Seq(rnd -> y))
  }
}
