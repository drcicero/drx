package drx

/** create a dynamic signal. you may call _.get on other signals inside the
  * closure to get and depend on their value. */
object Val {
  def apply[X](formula: => X, name: String = "")
              (implicit f: sourcecode.File, l: sourcecode.Line): Rx[X] = {
    val result = new InternalRx[X](nameit(name,f,l)) with Rx[X]
    result.formula = () => formula
    result
  }

  private[drx] def nameit(s: String, f: sourcecode.File, l: sourcecode.Line) = {
    val ff = f.value.substring(f.value.lastIndexOf("/")+1)
    val fff = ff.substring(0, ff.indexOf("."))
    s"${s}_$fff:${l.value}"
  }
}
