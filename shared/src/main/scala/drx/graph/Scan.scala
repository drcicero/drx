package drx.graph

import drx.Name

import scala.util.{Success, Try}

/** create a dynamic Scan. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Scan {
  def apply[X](init: X)(comb: X => X)(implicit n: Name): Rx[X] = {
    val result = new RxDynamic[X](true, n.toString, Success(init)) with Rx[X] {
      override protected[this] val formula: Try[X] => X = v => comb(v.get)
    }
    result.start()
    result
  }
}
