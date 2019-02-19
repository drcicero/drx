import java.util.concurrent.ThreadLocalRandom

import drx.{Rx, SeqVar, Var}
import upickle.default.{ReadWriter, Reader, Writer, macroRW, read, readwriter, write}

import scala.collection.mutable
import org.scalajs.dom.experimental.{Fetch, HttpMethod, RequestInit, Response}
import upickle.default

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.{DynamicVariable, Failure, Success, Try}

private sealed trait Msgi
private case class NoMsg() extends Msgi
private case class Listen(source: Network.ClientId, id: String) extends Msgi
private case class Change(source: Network.ClientId, id: String, payload: String) extends Msgi
private object Msgi   { implicit val rw: ReadWriter[Msgi] = ReadWriter.merge(NoMsg.rw, Listen.rw, Change.rw) }
private object NoMsg  { implicit val rw: ReadWriter[NoMsg] = macroRW }
private object Listen { implicit val rw: ReadWriter[Listen] = macroRW }
private object Change { implicit val rw: ReadWriter[Change] = macroRW }

private case class Offer[X](rx: Rx[X], id: String, e: ReadWriter[X], startup: () => X)

object Network {
  type ClientId = String

  import Msgi.rw

  var localId: ClientId = "bot-" + ThreadLocalRandom.current().nextLong().toHexString
//  fpost("./join", "abc").foreach(localId = _)

  // map remote to local var id, and otherwise
  private val offeredRx: mutable.Map[String, Offer[_]] = mutable.Map()
  private val publishedRx: mutable.Set[(Rx[_], ClientId)] = mutable.Set()
  private val outbox = SeqVar[(String, String)]()

  private val subscribedRx: mutable.Map[(ClientId, String), (Var[_], ReadWriter[_])] = mutable.Map()
  private val subscribedRxFromAll: mutable.Map[String, (Var[(ClientId, _)], ReadWriter[_])] = mutable.Map()
  private val notYetRx: mutable.Map[(ClientId, String), String] = mutable.Map()

  implicit def tryRW[X](implicit rw: ReadWriter[X]): ReadWriter[Try[X]] =
    readwriter[Either[String, X]].bimap[Try[X]](
      y => y.fold(x => Left(x.toString), Right(_)),
      y => y.fold(x => Try(sys.error(x)), Success(_))
    )

  implicit def varRW[X](implicit xRW: ReadWriter[X]): ReadWriter[Var[X]] =
    readwriter[(ClientId, String)].bimap[Var[X]](
      { y =>
        pub(Offer(y, y.hashCode().toString, xRW, ()=>y.get))
        (localId, y.hashCode().toString)
      },
      { case (remoteId, hashCode) =>
        subscribedRx.getOrElseUpdate(
          (remoteId, hashCode),
          { val remoteVar = if (notYetRx.contains((remoteId, hashCode))) {
            Var[X](read(notYetRx.remove((remoteId, hashCode)).get)(xRW))
          } else Var.mkEmptyVar[X]
            (remoteVar, xRW) }
        )._1.asInstanceOf[Var[X]]
      }
    )

  implicit def rxRW[X](implicit xRW: ReadWriter[X]): ReadWriter[Rx[X]] =
    readwriter[(ClientId, String)].bimap[Rx[X]](
      { y =>
        pub(Offer(y, y.hashCode().toString, xRW, ()=>y.get))
        (localId, y.hashCode().toString)
      },
      { case (remoteId, hashCode) =>
        subscribedRx.getOrElseUpdate(
          (remoteId, hashCode),
          { val remoteVar = if (notYetRx.contains((remoteId, hashCode))) {
            Var[X](read(notYetRx.remove((remoteId, hashCode)).get)(xRW))
          } else Var.mkEmptyVar[X]
            (remoteVar, xRW) }
        )._1.asInstanceOf[Rx[X]]
      }
    )

  /* publish a variable */
  var tos = new DynamicVariable[Set[ClientId]](Set())
  private def pub[X](offer: Offer[X]): Unit = {
    //if (ignore) return
    println(s"new pub ${offer.id} ${offer.rx}")
    tos.value.foreach { to =>
      if (publishedRx.add((offer.rx, to))) {
        tos.withValue(Set(to)) {
          outbox set (to, write(Change(localId, offer.id, write(offer.startup())(offer.e))))
        }
        val obs = offer.rx.foreach { y =>
          tos.withValue(Set(to)){
            println(s"send -> $to ${offer.id} $y")
            outbox set (to, write(Change(localId, offer.id, write(y)(offer.e))))
          }
        }
      }
    }
    // TODO connectionloss/timeout, delete obs
  }

  /* subscribe to a variable name & type */
  def sub[X](init: X, variname: String)
            (implicit readwriter: ReadWriter[X]): Rx[(ClientId, X)] = {
    val remoteVar = Var[(ClientId, X)](("", init))
    subscribedRxFromAll += variname -> (remoteVar.asInstanceOf[Var[(ClientId, _)]], readwriter)
    remoteVar
  }

  /* publish a variable */
  def offer[X](rx: Rx[X], startup: () => X, id: String)
              (implicit e: ReadWriter[X]): Unit = {
    if (offeredRx.contains(id)) return
    offeredRx(id) = Offer(rx, id, e, startup)
  }

  def startHeartbeat(): Unit = heartbeat().onComplete {
    case Success(s) => scala.scalajs.js.timers.setTimeout(500)(startHeartbeat())
    case Failure(e) => throw e
  }

  def fetchText(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.text.toFuture)

  // https://stackoverflow.com/questions/9267899/arraybuffer-to-base64-encoded-string/11562550#11562550
  def fetchBase64(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.arrayBuffer.toFuture).map { x =>
      val btoa = org.scalajs.dom.window.btoa _
      val fromCharCode = scalajs.js.Dynamic.global.String.fromCharCode
      btoa(fromCharCode.applyDynamic("apply")(
        (), new Uint8Array(x)).asInstanceOf[String])
    }

  def fpost(url: String, body: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.POST, body = body))
      .toFuture.flatMap(x => x.text.toFuture)

  /* --- private --- */

  private def sendOut(messagebuffer: Seq[Seq[(String, String)]]): Unit = {
    val tmp = messagebuffer.flatten
    if (tmp.isEmpty) return
    fpost("./push", write(tmp))
  }

  var known: Set[ClientId] = Set()
  private def heartbeat(): Future[Unit] = {
    fpost("./pull/" + localId, "").map { blob =>
      val (alive, news) = read[(Set[ClientId], Seq[String])](blob)

      val killed = known -- alive
      val added = alive -- known
      known = alive
      added.foreach ( newremote =>
        subscribedRxFromAll.foreach { case (variId, (vari, _)) =>
          outbox set (newremote, write(Listen(localId, variId)))
        }
      )

      news foreach { item => read[Msgi](item) match {
        case NoMsg() =>
          // nothing

        case Listen(remoteId, id) =>
          tos.withValue(Set(remoteId)) {
            offeredRx.get(id).foreach(pub(_))
          }

        case Change(remoteId, variId, message) =>
          println(s"  recv $remoteId $message -> $variId")
          if (subscribedRxFromAll.contains(variId))
            subscribedRxFromAll.get(variId).foreach(s =>
              s._1.asInstanceOf[Var[(ClientId, Any)]] set (remoteId, read(message)(s._2))) // update remote vars
          else if (subscribedRx.contains((remoteId, variId)))
            subscribedRx.get((remoteId, variId)).foreach(s =>
              s._1.asInstanceOf[Var[Any]] set read(message)(s._2)) // update remote vars
          else notYetRx((remoteId, variId)) = message
      }
    } }
  }

  def bufferTime[X](incoming: Rx[X], before: Int, between: Int)
                   (implicit readWriter: ReadWriter[X]): Var[Seq[X]] = {
    val buffer = mutable.ListBuffer[X]()
    var running = false
    val outgoing = Var[Seq[X]](Seq[X]())

    def f(): Unit = {
      if (buffer.nonEmpty) {
        val tmp = Seq() ++ buffer
        buffer.clear()
        outgoing set tmp
        scala.scalajs.js.timers.setTimeout(between){ f() }
      } else {
        running = false
      }
//      println(s"buffer $running $buffer")
    }

    incoming foreach { x =>
      buffer ++= Seq(x)
      if (!running) {
        running = true
        scala.scalajs.js.timers.setTimeout(before){ f() }
      }
    }

    outgoing
  }

  //private var ignore = false
  private var watermark = 0

  bufferTime(outbox, 100, 500) foreach sendOut
}

//    type PreDelta = (Set[Task], Set[Task])
//    type Delta = (Iterable[Task.DeserT], Seq[String])

//    private val msgctrs = mutable.Map[String, Int]()
//    private val incoming = mutable.Map[String, mutable.Map[(String, String), Var[_]]]()
//    private val subscriptionlist = mutable.ListBuffer[String]()

//    private var outbox = mutable.Map[String, (Seq[Delta], Map[String, String])]()
//    private val inbox = new Var[String](write(outbox.toMap),"SEND")

//    private def enqueueMsg(name: String, delta: PreDelta): Unit = {
//      val (plus, minus) = delta
//      val minus2 = minus.map(_.id).toSeq
//      val plus2  = plus.map(it => it.sampleTuple())
//      val delta2 = (plus2, minus2)

//      val (olddelta, m) = outbox.getOrElse(name, (Seq(), Map[String,String]()))
//      outbox(name) = (olddelta ++ Seq(delta2), m)
//      plus map { it =>
//        it.title observe { valu => 
//          val tup = it.title.id -> write(valu)
//          val (a, b) = outbox.getOrElse(name, (Seq(), Map[String,String]()))
//          outbox(name) = (a, b ++ Seq(tup))
//          println(s"send $tup")
//        }
//        it.done observe { valu => 
//          val tup = it.done.id -> write(valu)
//          val (a, b) = outbox.getOrElse(name, (Seq(), Map[String,String]()))
//          outbox(name) = (a, b ++ Seq(tup))
//          println(s"send $tup")
//        }
//      }
//    }

//    def publish(store: Store[Task,_], localname: String): Unit = {
////      enqueueMsg(localname, (None, store.sample))
//      store.diffs.observe { delta => enqueueMsg(localname, delta) }
//    }

//    def subscribe(remote: String, store: Store[Task,Task.CtorT], local: String): Unit = {
//      subscriptionlist += remote
//      inbox observe { blob =>
//        val map = read[Map[String, (Int, String)]](blob)
//        if (map contains remote) {
//          val (msgctr, value) = map(remote)
//          println("msgctrs " + remote + ", " + msgctrs.getOrElse(remote, -1) +" < "+ msgctr)
//          if (msgctrs.getOrElse(remote, -1) < msgctr) {
//            msgctrs(remote) = msgctr
//            val (metas, messages) = read[(Seq[Delta], Map[String, String])](value)
//            val lincoming = incoming.getOrElseUpdate(local, mutable.Map())
//            var messages2 = Map[String, String]()

//            if (metas.size>0) {
//              val sample   = store.sample.toSet
//              val plus     = metas.flatMap(_._1).toMap
//              val minus    = metas.flatMap(_._2).toSet

//              val addition = plus -- sample.map(_.id)
//              val update   = plus -- addition.map(_._1)
//              val subtract = sample filter (i => minus contains i.id)
//              val update2  = sample filter (i => update contains i.id)

//              val delta = (addition.map(it => (it._1, read[String](it._2(0)._2), read[Boolean](it._2(1)._2)).asInstanceOf[Task.CtorT]), subtract)
//              val vars  = store.update(delta)
//              val left  = addition.flatMap(it => Seq(it._2(0)._1, it._2(1)._1))
//              val right = vars.flatMap(it => it.getVars)
//              val updateleft = update.toSeq.sortBy(_._1).flatMap(it => Seq((remote, it._2(0)._1), (remote, it._2(1)._1)))
//              val updateright = update2.toSeq.sortBy(_.id).flatMap(_.getVars)

//              if (left.size != right.size || updateleft.size != updateright.size)
//                throw new RuntimeException("bad")
//              lincoming --= subtract.flatMap(_.getVars.map(it => (remote, it.id)))
//              lincoming ++= left.map(it => (remote, it)) zip right
//              lincoming ++= updateleft zip updateright

//              messages2 = update.flatMap(it =>
//                Seq(it._2(0)._1 -> it._2(0)._2, it._2(1)._1 -> it._2(1)._2)).toMap
//            }

//            (messages2 ++ messages) foreach { case (name, value) =>
//              val vari = lincoming((remote, name))
//              println(s"  recv $name -> $value -> ${vari.id}")
//              vari parse value
//            }
//          }
//        }
//      }
//    }

