package drx.interface

object DSL extends DSL {
  var innerdsl: DSL = _
  // final type Obs = Val[Unit]

  override def Val[X](e: => X): Val[X] = innerdsl.Val(e)
  override def Var[X](e: => X): Var[X] = innerdsl.Var(e)
  override def MultiVar[X](): MultiVar[X] = innerdsl.MultiVar()
  override def forceTick(): Unit = innerdsl.forceTick()

  var transactionActive = 0
  def transact[X](f: => X): X = {
    transactionActive += 1
    val x = f
    transactionActive -= 1
    if (transactionActive == 0)
      forceTick()
    x
  }

  abstract class Val[+X](dsl: DSL) {
    def get: X
    def sample: X
    def forceStart(): Unit
    def forceStop(): Unit
    def scan[Y](init: Y)(f: (Y, X) => Y): Val[Y]

    def zip[Y](y: Val[Y]): Val[(X,Y)] = dsl.Val((get, y.get))
    def map[Y](f: X => Y): Val[Y] = dsl.Val(f(get))
    def foreach(f: X => Unit): Val[Unit] = {
      val result = dsl.Val(f(get))
      result.forceStart() // leaky!
      result
    }
  }

  abstract class Var[X](dsl: DSL) extends Val[X](dsl) {
    def set(xnew: => X): Unit
    def transform(f: X => X): Unit = { val next = f(get); set(next) }
  }

  abstract class MultiVar[X](dsl: DSL) extends Val[Seq[X]](dsl) {
    def set(newValue: X): Unit
  }

  case class Polarized[X](polarity: Boolean, content: X)

  final class VarMap[X] {
    val diffs: MultiVar[(String, Polarized[X])] = MultiVar[(String, Polarized[X])]()
    val aggregate: Val[Map[String, X]] = diffs.scan(Map[String, X]())(VarMap.add[X])
    def sampleAsDelta: Seq[(String, Polarized[X])] = aggregate.sample.mapValues(x => Polarized(true, x)).toSeq
    def update(delta: Seq[(String, Polarized[X])]): Unit = delta.foreach(diffs.set)
    def remove(delta: Seq[String]): Unit = { val tmp = aggregate.sample; update(delta.map(k => (k, Polarized(false, tmp(k))))) }
    def keep(func: X => Boolean): Unit = remove(aggregate.sample.filter(x => func(x._2)).keys.toSeq)
  }

  object VarMap {
    def apply[X]() = new VarMap[X]()
    def add[X](x: Map[String, X], y: Seq[(String, Polarized[X])]): Map[String, X] = {
      (x.mapValues(x => Polarized(true, x)) ++ y) collect
        { case (k, Polarized(true, v)) => (k, v) }
    }
  }

  implicit class ImapmapValues[X,Y](val rx: Val[TraversableOnce[(String, Polarized[X])]]) extends AnyVal {
    def mapmapValues(fun: X => Y): Val[TraversableOnce[(String, Polarized[Y])]] =
      rx.map { seq => seq map { case (k, Polarized(polarity, v)) => k -> Polarized(polarity, fun(v)) } }
  }
}

trait DSL {
  def Val[X](e: => X): DSL.Val[X] // signals
  def Var[X](e: => X): DSL.Var[X] // changable, should we provide expr or val?
  def MultiVar[X](): DSL.MultiVar[X] // events
  def forceTick(): Unit
}
