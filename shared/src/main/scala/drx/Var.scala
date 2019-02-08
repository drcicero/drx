package drx

import upickle.default._

import scala.collection.{GenTraversableOnce, mutable}
import scala.util.{Failure, Success, Try}

object Var {
  val incoming: mutable.Map[String, Var[_]] = mutable.Map()

  implicit def varRW[X](implicit e: ReadWriter[X]): ReadWriter[Var[X]] =
    readwriter[String].bimap[Var[X]](
      y => write((y.id, y.unparse())),
      y => { val (id, valu) = read[(String, String)](y)
             incoming.getOrElseUpdate(id, new Var[X](read[X](valu), id))
               .asInstanceOf[Var[X]] }
    )

  implicit def tryRW[X](implicit rw: ReadWriter[X]): ReadWriter[Try[X]] =
    readwriter[Either[String, X]].bimap[Try[X]](
      y => y.fold(x => Left(x.toString), Right(_)),
      y => y.fold(x => Try(sys.error(x)), Success(_))
    )

  def mkEmptyVar[X](name: String = "")
                   (implicit readwriter: ReadWriter[X]): Var[X] = {
    val vari = new Var[X](null.asInstanceOf[X], name)
    vari.underlying.value = internals.emptyValExc()
    vari
  }
}

class Var[X](protected var buffer: X, name: String = "")
            (implicit val rw: ReadWriter[X])
            extends ProxyRx[X](name) {

  underlying.remember = true
  underlying.value = Success(buffer)
  underlying.formula = () => buffer
  underlying.forceActive = true

  val diffs: Var[X] = this

  def set(newValue: X): Unit = {
    if (newValue != buffer) {
      buffer = newValue
      withInstant(_.markSource(underlying))
    }
  }

  def transform(transformer: X => X): Unit = set(transformer(underlying.value.get))

  def parse(str: String): Unit = set(read[X](str))
  def unparse(): String = write(buffer)
//  override /*private[drx]*/ def getVars: Signal[Seq[Var[_]]] = new Var(Seq(this))

  protected def getEmbeddedVaris(valu: X): GenTraversableOnce[Var[_]] = valu match {
    case x: Var[_] => Option(x)
    case _ => None
  }

  def mkStrObs(callback: (Seq[Var[_]],String) => Unit)
              (implicit f: sourcecode.File, l: sourcecode.Line): Obs[Unit] = {
    val result = new InternalRx[Unit](Val.nameit("o",f,l)) with Obs[Unit]
    result.formula = () => {
      val valu = Try(this.underlying.getValue)
      val tmp = Seq(this) ++ valu.toOption.toSeq.flatMap(it => getEmbeddedVaris(it))
      val tmp2 = internals.activeRx.withValue(None)( write(valu)(Var.tryRW) )
      withInstant(_.runLater{ () => callback(tmp, tmp2) })
      Try(())
    }
    result
  }

  def strobserve(callback: (Seq[Var[_]],String) => Unit)
                (implicit f: sourcecode.File, l: sourcecode.Line): Obs[Unit] = {
    val result = mkStrObs(callback)(f, l)
    result.start()
    result
  }
}

// TODO make observers and sources declarative by collecting them if unreachable
// turn all Sources & Sinks (Folds+Observers)
// into proxies/wrappers that only weakref the real Rx.
// Then we can kill/stop them in the finalizer.
class ProxyRx[X](name: String) extends Rx[X] {
  /*private[drx]*/ var underlying: InternalRx[X] = new InternalRx[X](name)
  override def id: String = underlying.id
  override def finalize(): Unit = { underlying.freeze(); underlying = null }
}

// TODO parallel
// each change may start its own transaction. globally lock write levels.
// next transactions may begin running on levels that are lower than still
// running past transactions.

// TODO time travel
// save serializable list of transactions

//  // TODO async call value
//object FutureWork {
//  def async[X,Y](rx: Rx[X], init: Y, f: (X)=>Y): Rx[Y] = {
//    val input = rx.underlying.value
//    val result = new Var[Y](init)
//    runInThread { () =>
//      val value = f(input)
//      result.set(value)
//    }
//    result
//  }
//}
