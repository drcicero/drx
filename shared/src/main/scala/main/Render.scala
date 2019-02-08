/** Created by david on 10.06.17. */
package main

import drx.{Rx, Var}
import upickle.default._

case class Task(title: Var[String], done: Var[Boolean]) {
  private val folded: Rx[Int] = title.map(_+1).scan(0)((state, event) => state + 1)
}
object Task {
  def mk(title: String) = Task(new Var(title), new Var(false))
  implicit val rwTask: ReadWriter[Task] = macroRW
//    readwriter[(String, Boolean)].bimap[Task](Task.unapply, Task.apply)
//  implicit val rwTasks: ReadWriter[List[Task]] = macroRW
//    readwriter[Seq[Option[(Var[String], Var[Boolean])]]]].bimap[Seq[Task]](
//      it => it.map(Task.unapply),
//      it => it.map(it => Task.apply(it._1, it._2)))
}
