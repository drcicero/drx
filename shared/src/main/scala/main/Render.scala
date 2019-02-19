/** Created by david on 10.06.17. */
package main

import drx.{Rx, Var}
import upickle.default._

case class Task(title: Var[String], done: Var[Boolean]) {
  private val folded: Rx[Int] = title.map(_+1).scan(0)((state, event) => state + 1)
}

object Task {
  def mk(title: String) = Task(
    Var(title),
    Var(false))

//  implicit val rwTask: ReadWriter[Task] = macroRW
}
