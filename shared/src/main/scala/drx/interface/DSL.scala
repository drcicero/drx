package drx.interface

object DSL extends DSL {
  // proxy
  var innerdsl: DSL = _
  override def RawVar[I,O](init: Seq[I], f: Seq[I] => O): DSL.RawVar[I,O] = innerdsl.RawVar(init, f)
  override def Val[X](e: => X): Val[X] = innerdsl.Val(e)
  override protected def forceStep(): Unit = innerdsl.forceStep()

  // sugar

  type Var[X] = RawVar[X,X]
  type SeqVar[X] = RawVar[X,Seq[X]]

  def Var[X](e: => X): Var[X] = innerdsl.RawVar[X,X](Seq(e), _.last)
  def SeqVar[X](): SeqVar[X] = innerdsl.RawVar(Seq(), x => x)

  var transactionActive = 0
  def transact[X](f: => X): X = {
    transactionActive += 1
    val x = f
    if (transactionActive == 1) forceStep()
    transactionActive -= 1
    x
  }

  abstract class Val[+O](dsl: DSL) {
    def sample: O

    def get: O
    def enable(): Unit
    def disable(): Unit

    def scan[Y](init: Y)(f: (Y, O) => Y): Val[Y]
    def zip[Y](y: Val[Y]): Val[(O,Y)] = dsl.Val((get, y.get))
    def map[Y](f: O => Y): Val[Y] = dsl.Val(f(get))
    def foreach(f: O => Unit): Val[Unit] = {
      val result = dsl.Val(f(get))
      result.enable() // leaky!
      result
    }
  }

  abstract class RawVar[I,O](reduce: Seq[I]=>O, dsl: DSL) extends Val[O](dsl) {
    def set(newValue: I): Unit
    def transform(f: O => I): Unit = { val x = f(get); set(x) }
  }

  sealed abstract class Polarized[X]
  case class Pos[X](content: X) extends Polarized[X]
  case class Neg[X](content: X) extends Polarized[X]

  final class VarMap[X] {
    val diffs: SeqVar[(String, Polarized[X])] = SeqVar[(String, Polarized[X])]()
    val aggregate: Val[Map[String, X]] = diffs.scan(Map[String, X]())(VarMap.add[X])
    def sampleAsDelta: Seq[(String, Polarized[X])] = aggregate.sample.mapValues(x => Pos(x)).toSeq
    def update(delta: Seq[(String, Polarized[X])]): Unit = delta.foreach(diffs.set)
    def remove(delta: Seq[String]): Unit = { val tmp = aggregate.sample; update(delta.map(k => (k, Neg(tmp(k))))) }
    def keep(func: X => Boolean): Unit = remove(aggregate.sample.filter(x => func(x._2)).keys.toSeq)
  }

  object VarMap {
    def apply[X]() = new VarMap[X]()
    def add[X](x: Map[String, X], y: Seq[(String, Polarized[X])]): Map[String, X] = {
      (x.mapValues(x => Pos(x)) ++ y) collect
        { case (k, Pos(v)) => (k, v) }
    }
  }

  implicit class ImapmapValues[X,Y](val rx: Val[TraversableOnce[(String, Polarized[X])]]) extends AnyVal {
    def mapmapValues(fun: X => Y): Val[TraversableOnce[(String, Polarized[Y])]] =
      rx.map { seq => seq map {
        case (k, Pos(v)) => k -> Pos(fun(v))
        case (k, Neg(v)) => k -> Neg(fun(v))
      } }
  }
}

trait DSL {
  def RawVar[I,O](init: Seq[I], f: Seq[I] => O): DSL.RawVar[I,O] // events
  def Val[O](e: => O): DSL.Val[O] // signals
  protected def forceStep(): Unit
}
