/** Created by david on 10.06.17. */
package main

import drx.{Signal, Variable, VarOwner}

class Task(title_ : String) extends VarOwner {
  val title: Variable[String] = mkVar(title_, "t")
  val done: Variable[Boolean] = mkVar(false, "d")
  val folded: Signal[Int] = title.map(_+1).fold(0, this)((x, y) => x + 1)
}
