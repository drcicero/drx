package drx.graph

import drx.Name

import scala.util.Try

/** create a dynamic signal. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Val {
  def apply[X](func: => X)(implicit n: Name): Rx[X] =
    new DynamicRx[X](false, n.toString) with Rx[X] {
      override protected[this] val formula: Try[X] => X = _ => func
    }
}
