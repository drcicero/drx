package drx

sealed class IncMap[X](implicit n: Name) {

  val diffs: SeqVar[(String, X)] = new SeqVar[(String, X)]()(n)

  val aggregate: Rx[Map[String, X]] = diffs
    .scan(Map[String, X]()) { (x, y) =>
//      println("aggregate" + (x, y))
      (x ++ y) filter {  _._2 != null}
    }

  def update(delta: Seq[(String,X)]): Unit = atomic { diffs.set(delta) }

}
