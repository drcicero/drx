package standard

import scala.collection.mutable

object PullDSL extends DSL with SDomHelper {
  final class SVal[+X](e: () => X) extends Val[X] {
    override def get: X = { val x = e(); println("get " + x); x }
    override def sample: X = e()
  }

  trait Touchable {
    protected[PullDSL] def touch(): Unit
  }
  final class SVar[X](start: () => X) extends Var[X] with Touchable {
    private var curr: () => X = start
    private var next: () => X = start
    override def get: X = curr()
    override def sample: X = curr()
    override def set(e: => X): Unit = { next = e _; transact(dirtyVars.add(this)) }
    override protected[PullDSL] def touch(): Unit = { println("set" + (curr, next)); curr = next }
  }

  final class SMultiVar[X] extends MultiVar[X] with Touchable {
    private var curr = Seq[X]()
    private val next: mutable.Buffer[X] = mutable.Buffer[X]()
    override def get: Seq[X] = curr
    override def sample: Seq[X] = curr
    override def set(newValue: X): Unit = { next ++= Seq(newValue); transact(dirtyVars.add(this)) }
    override protected[PullDSL] def touch(): Unit = { println("mset " + (curr, next)); val tmp = Seq() ++ next; next.clear(); curr = tmp }
  }

  private val dirtyVars = mutable.Set[Touchable]()
  private val forceObs = mutable.Set[Val[_]]()

  override def Val[X](e: => X) = new SVal(e _)
  override def Var[X](e: => X) = new SVar(e _)
  override def MultiVar[X]() = new SMultiVar[X]()

  override def forceStart[X](valu: Val[X]): Unit = { forceObs += valu }
  override def forceStop[X](valu: Val[X]): Unit = { forceObs -= valu }
  override def forceTick(): Unit = {
    println((dirtyVars, forceObs))
    dirtyVars.foreach(_.touch())
    forceObs.foreach(_.sample)
  }


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
  implicit class ValDMapMap[X,Y](val rx: Val[TraversableOnce[(String, Polarized[X])]]) extends AnyVal {
    def mapmapValues(fun: X => Y): Val[TraversableOnce[(String, Polarized[Y])]] =
      rx.map { seq => seq map { case (k, Polarized(polarity, v)) => k -> Polarized(polarity, fun(v)) } }
  }
}
