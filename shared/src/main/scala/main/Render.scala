/** Created by david on 10.06.17. */
package main

import drx.{Signal, Var}
//import rescala.Engines.{logging => rescala}; import rescala._

class Task(title_ : String) {
  val title: Var[String] = new Var(title_)
  val done: Var[Boolean] = new Var(false)
}
