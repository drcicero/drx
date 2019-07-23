package drx.interface

object dummy extends DSL {
  override def Val[X](e: => X): DSL.Val[X] = ???
  override def Var[X](e: => X): DSL.Var[X] = ???
  override def MultiVar[X](): DSL.MultiVar[X] = ???
  override def forceTick(): Unit = ???
}
