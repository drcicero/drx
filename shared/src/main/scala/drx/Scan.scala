package drx

import scala.util.{Success, Try}

object Scan {
  def apply[X](init: X)(comb: X => X, name: String = "")
              (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](Val.nameit("f",f,l)) with Rx[X]
    result.remember = true
    result.value = Success(init)
    result.formula = () => comb(result.value.get)
    result.observe { _ => }
    result
  }
}
