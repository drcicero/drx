package drx

import upickle.default._

import scala.collection.GenTraversableOnce

sealed class IncMap[X <: Product](name: String = "")
                                 (implicit e: ReadWriter[Seq[(String,X)]])
{
//  type StringMapDelta[X] = Seq[(String, X)]
//  implicit val deltaRW: ReadWriter[StringMapDelta[X]] = e
//  implicit def deltaRW[X](implicit e: ReadWriter[Seq[(String, X)]]): ReadWriter[StringMapDelta[X]] = macroRW

  val diffs: Var[Seq[(String, X)]] = new Var[Seq[(String, X)]](Seq(), name=name) {
    override protected def getEmbeddedVaris(valu: Seq[(String, X)]): GenTraversableOnce[Var[_]] = {
      valu.flatMap { case (_, valu) =>
        (if (valu == null) Seq() else
          for (i <- 0 until valu.productArity)
            yield valu.productElement(i)
          ).collect { case v: Var[_] => v }
      }
    }

    underlying.formula = () => {
      val tmp = buffer
      buffer = Seq()
      tmp
    }

    override def set(delta: Seq[(String, X)]): Unit = {
      if (delta.nonEmpty) {
        buffer ++= delta
        withInstant(_.ensureSource(underlying))
      }
    }
  }

  lazy val aggregate: Rx[Map[String, X]] = diffs
    .scan(Map[String, X](), "") { (x, y) =>
      println("aggregate" + (x, y))
      (x ++ y) filter {  _._2 != null}
    }

  def update(delta: Seq[(String,X)]): Unit = diffs.set(delta)
}
