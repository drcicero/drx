/** Created by david on 10.06.17. */
package main

import drx.{Signal, Variable, VariableLike}
//import rescala.Engines.{logging => rescala}; import rescala._

class Task(title_ : String) extends VariableLike {
  val title: Variable[String] = new Variable(title_, "t")
  val done: Variable[Boolean] = new Variable(false, "d")
  val folded: Signal[Int] = title.fold(0)((x, y) => x + 1)

  override def getVariables: Seq[Variable[_]] = Seq(title, done)
}
