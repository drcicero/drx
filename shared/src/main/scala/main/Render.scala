/** Created by david on 10.06.17. */
package main

import drx.{DynamicSignal, Variable, VarOwner, Signal}
//import rescala.Engines.{logging => rescala}; import rescala._

class Task(title_ : String) extends VarOwner {
  val title: Variable[String] = mkVar(title_, "t")
  val done: Variable[Boolean] = mkVar(false, "d")
  val folded: DynamicSignal[Int] = title.fold(0, this)((x, y) => x + 1)
}
