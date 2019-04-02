package drx

import drx.graph.{Rx, VarSeq, Var}

//sealed class ApplySeqFold[X](init: X)(implicit n: Name) {
//  val diffs: SeqVar[X => X] = new VarSeq[X => X]()(n)
//  val aggregate: Rx[X] = diffs
//    .scan(init) { (x, ff) => ff.foldLeft(x) { (y, f) => f(y) } } // TODO see comment below
//  def update(delta: Seq[X => X]): Unit = diffs.set(delta)
//}

// usage:
//   val state = ApplyFold(Map[Int, Int])
//   state.observe(println)
//   state.update(_ ++ Seq(1->2, 3->4))
sealed class ApplyFold[X](init: X)(implicit n: Name) {
  val diffs: Var[X => X] = Var.mkEmptyVar(n)
  val aggregate: Rx[X] = diffs.scan(init) { (x, ff) =>
    internals.activeRx.withValue(None){ ff(x) } } // do not make dependencies
  def update(delta: X => X): Unit = diffs.set(delta)
}

sealed class IncMap[X](implicit n: Name) {
  val diffs: VarSeq[(String, X)] = new VarSeq[(String, X)]()(n)
  val aggregate: Rx[Map[String, X]] = diffs
    .scan(Map[String, X]()) { (x, y) =>
      (x ++ y) filter { _._2 != null }
    }
  def update(delta: Seq[(String,X)]): Unit = diffs.set(delta)
  def remove(delta: Seq[String]): Unit = diffs.set(delta.map(_ -> null.asInstanceOf[X]))
  def remove(func: X => Boolean): Unit = remove(aggregate.sample.filter(x => !func(x._2)).keys.toSeq)
}
