import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.commands.Commands

import scala.util.Try

trait Bisimulation[X] extends Commands {
  type Base
  type State <: Base
  type Sut <: Base

  def get(b: Base): X
  def copy(s: State): State

  def cmd(f: Base => Unit, n: String): Command = new Command {
    override type Result = X
    override def run(sut: Sut): Result = { f(sut); get(sut) }
    override def nextState(state: State): State = { val nstate = copy(state); f(nstate); nstate }
    override def preCondition(state: State): Boolean = true
    // warning: postConditions gets PREVIOUS STATE and current result.
    override def postCondition(state: State, result: Try[Result]): Prop = {
      val a = result.get
      val b = get(nextState(state))
      s"assert $a == $b" |: a == b
    }
    override def toString: String = n
  }

  // loop { if canCreateNewSut(...) { val x = genInitialState; if initialPreCondition(x) newSut(x) } }
  def canCreateNewSut(newState: State, initSuts: Traversable[State],
                      runningSuts: Traversable[Sut]): Boolean = true
  def initialPreCondition(state: State): Boolean = true
  def destroySut(sut: Sut): Unit = ()
}
