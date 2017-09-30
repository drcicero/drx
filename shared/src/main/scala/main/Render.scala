/** Created by david on 10.06.17. */
package main

import drx.{Rx, VarOwner, Variable}

class Task(title_ : String) extends VarOwner {
  val title: Variable[String] = mkVar(title_, "t")
  val done: Variable[Boolean] = mkVar(false, "d")
  private val folded: Rx[Int] = title.map(_+1).fold(0)((state, event) => state + 1)
}
