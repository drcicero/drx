package standard

trait DSL {
  trait Val[+X] {
    def get: X
    def sample: X

    def zip[Y](y: Val[Y]): Val[(X,Y)] = Val((get, y.get))
    def map[Y](f: X => Y): Val[Y] = Val(f(get))
    def foreach(f: X => Unit): Val[Unit] = {
      val result = Val(f(get))
      forceStart(result) // leaky!
      result
    }
    def scan[Y](init: Y)(comb: (Y, X) => Y): Val[Y] = {
      var x = init
      Val { x = comb(x, get); x }
    }
  }

  trait Var[X] extends Val[X] {
    def set(xnew: => X): Unit
    def transform(f: X => X): Unit = { val next = f(get); set(next) }
  }

  trait MultiVar[X] extends Val[Seq[X]] {
    def set(newValue: X): Unit
  }

  def Val[X](e: => X): Val[X]
  def Var[X](e: => X): Var[X]
  def MultiVar[X](): MultiVar[X]
  type Obs = Val[Unit]

  def forceStart[X](valu: Val[X]): Unit
  def forceStop[X](valu: Val[X]): Unit
  def forceTick(): Unit

  var transactionActive = 0
  def transact[X](f: => X): X = {
    transactionActive += 1
    val x = f
    transactionActive -= 1
    if (transactionActive == 0)
      forceTick()
    x
  }

  case class Polarized[X](polarity: Boolean, content: X)
}
