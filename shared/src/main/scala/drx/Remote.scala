package drx

import java.util.concurrent.ThreadLocalRandom

import concreteplatform.executionContext
import drx.graph.{Obs, Rx, VarSeq, Var}
import upickle.default
import upickle.default.{ReadWriter, macroRW, read, readwriter, write}

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{DynamicVariable, Failure, Success, Try}

object Remote {
  private sealed trait Msgi
  private case class Listen(source: Remote.ClientId, id: String, sync: Boolean) extends Msgi
  //private case class UnListen(source: Network.ClientId, id: String, sync: Boolean) extends Msgi
  private case class Change(source: Remote.ClientId, id: String, payload: String, sync: Boolean) extends Msgi
  private case class Quiescent(source: Remote.ClientId) extends Msgi
  private object Msgi       { implicit val rw: ReadWriter[Msgi] = ReadWriter.merge(Listen.rw, Change.rw, Quiescent.rw) }
  private object Listen     { implicit val rw: ReadWriter[Listen] = macroRW }
  //private object UnListen   { implicit val rw: ReadWriter[Listen] = macroRW }
  private object Change     { implicit val rw: ReadWriter[Change] = macroRW }
  private object Quiescent  { implicit val rw: ReadWriter[Quiescent] = macroRW }

  private case class Offer[X](rx: Rx[X], id: String, e: ReadWriter[X], startup: () => X)
  private case class Sub[X](rx: Var[_], e: ReadWriter[_], sync: Boolean)
  private case class SubAll[X](rx: Var[(ClientId, _)], e: ReadWriter[_], sync: Boolean)

  type ClientId = String

  import Msgi.rw

  var thisClient: ClientId = "bot-" + ThreadLocalRandom.current().nextInt().toHexString
//  fpost("./join", "abc").foreach(localId = _)

  // map remote to local var id, and otherwise
  private val offeredRx: mutable.Map[String, Offer[_]] = mutable.Map()
  private val publishedRx: mutable.Map[(Rx[_], ClientId), Obs[_]] = mutable.Map()
  private val outbox = mutable.ArrayBuffer[(String, Msgi)]()

  private val subscribedRx: mutable.Map[(ClientId, String), Sub[_]] = mutable.Map()
  private val subscribedRxFromAll: mutable.Map[String, SubAll[_]] = mutable.Map()
  private val notYetRx: mutable.Map[(ClientId, String), String] = mutable.Map()

  def sharedAs[X](sig: Rx[X], id: String, startup: () => X, sync: Boolean = false)
                 (implicit e: ReadWriter[X]): Rx[(ClientId, X)] = {
    offer(sig, startup, id)
    sub[X](sig.sample, id, sync=sync)
  }

  def startHeartbeat(): Unit = heartbeat().onComplete {
    case Success(_) => concreteplatform.after(500)(startHeartbeat)
    case Failure(e) => throw e
  }

  /* --- private --------------------------------------- */

  implicit def tryRW[X](implicit rw: ReadWriter[X]): ReadWriter[Try[X]] =
    readwriter[Either[String, X]].bimap[Try[X]](
      y => y.fold(x => Left(x.toString), Right(_)),
      y => y.fold(x => Try(sys.error(x)), Success(_))
    )

  /* subscribe to a variable name & type */
  private def sub[X](init: X, variname: String, sync: Boolean)
                    (implicit readwriter: ReadWriter[X]): Rx[(ClientId, X)] = {
    val remoteVar = Var[(ClientId, X)]((thisClient, init))
    subscribedRxFromAll += variname -> SubAll(remoteVar.asInstanceOf[Var[(ClientId, _)]], readwriter, sync)
    remoteVar
  }

  /* publish a variable */
  private def offer[X](rx: Rx[X], startup: () => X, id: String)
                      (implicit e: ReadWriter[X]): Unit = {
    if (offeredRx.contains(id)) return
    offeredRx(id) = Offer(rx, id, e, startup)
  }

  private def getOrCreateVar[X](xRW: default.ReadWriter[X], remoteId: ClientId, hashCode: String) = {
    subscribedRx.getOrElseUpdate(
      (remoteId, hashCode),
      Sub(notYetRx
        remove (remoteId, hashCode)
        map { it =>
          val remoteVar = Var[X](read(it)(xRW))
          remoteVar
        }
        getOrElse Var.mkEmptyVar[X],
        xRW, sync=false)
    ).rx.asInstanceOf[Var[X]]
  }

  implicit def varRW[X](implicit xRW: ReadWriter[X]): ReadWriter[Var[X]] =
    readwriter[(ClientId, String)].bimap[Var[X]](
      { y =>
        pub(Offer(y, y.hashCode().toString, xRW, ()=>y.get))
        (thisClient, y.hashCode().toString)
      },
      { case (remoteId, hashCode) => getOrCreateVar(xRW, remoteId, hashCode) }
    )

  implicit def rxRW[X](implicit xRW: ReadWriter[X]): ReadWriter[Rx[X]] =
    readwriter[(ClientId, String)].bimap[Rx[X]](
      { y =>
        pub(Offer(y, y.hashCode().toString, xRW, ()=>y.get))
        (thisClient, y.hashCode().toString)
      },
      { case (remoteId, hashCode) => getOrCreateVar(xRW, remoteId, hashCode) }
    )

  /* publish a variable */
  private val pubConf = new DynamicVariable[Option[(ClientId, Boolean)]](None)
  private def pub[X](offer: Offer[X]): Unit = {
    //if (ignore) return
    pubConf.value foreach { case (to, sync) =>
      if (!publishedRx.contains((offer.rx, to))) {
        println(s"new pub ${offer.id} ${offer.rx} $to")
        enqueueChange(offer, to, sync, offer.startup())
        val remoteObs = offer.rx
          .map /* map vs foreach? */ { value =>
            pubConf.withValue(Some((to, sync))) {
              enqueueChange(offer, to, sync, value) }}
          .mkForeach(_ => ())
        publishedRx((offer.rx, to)) = remoteObs
        drx.withInstant(_.runLater(() => remoteObs.start()))
      }
    }
  }

  private def enqueueChange[X](offer: Offer[X], to: ClientId, sync: Boolean, value: X): Unit = {
    println(s"send -> $to ${offer.id} $value")
    val w = write(value)(offer.e)
    val c = Change(thisClient, offer.id, w, sync)
    if (sync) drx.atomic.waitingFor += to
    appendOutbox(to, c)
  }

  var knownClients: Set[ClientId] = Set()
  private def heartbeat(): Future[Unit] = {
    concreteplatform.fpost(url="./pull/" + thisClient, body="").map { blob =>
      println("recv <-- " +   blob)
      val (aliveClients, news) = read[(Set[ClientId], Seq[String])](blob)

      val killedClients = knownClients -- aliveClients
      val addedClients = aliveClients -- knownClients

      publishedRx.foreach { case ((rx, to), obs) =>
        if (killedClients contains to) obs.stop() } // TODO untested

      knownClients = aliveClients
      addedClients.foreach { newremote =>
        subscribedRxFromAll.foreach { case (variId, SubAll(_, _, sync)) =>
          println(s"listen -> $newremote $variId")
          appendOutbox(newremote, Listen(thisClient, variId, sync)) } }

      news foreach { item => println("  recv " + item) }

      news foreach { item => read[Msgi](item) match {
        case Listen(remoteId, id, sync) =>
          pubConf.withValue(Some((remoteId, sync))) {
            offeredRx get id foreach (pub(_)) }

        case Change(remoteClient, variId, message, sync) =>
          if (subscribedRxFromAll contains variId)
            subscribedRxFromAll get variId foreach (s =>
              s.rx set (remoteClient, read(message)(s.e))) // update remote vars
          else if (subscribedRx contains (remoteClient, variId))
            subscribedRx get (remoteClient, variId) foreach (s =>
              s.rx.asInstanceOf[Var[Any]] set read(message)(s.e)) // update remote vars
          else
            notYetRx((remoteClient, variId)) = message

          println(s"    recv ch $sync")
          if (sync) drx.withInstant(_.runBeforeWait { () =>
            println(s"s quiescent $thisClient -> $remoteClient")
            appendOutbox(remoteClient, Quiescent(thisClient))
          })

        case Quiescent(remoteClient) =>
          drx.atomic.waitingFor remove remoteClient
          println(s"    r quiescent ${drx.atomic.waitingFor}")
          if (drx.atomic.waitingFor.isEmpty)
            drx.atomic(()) // empty transaction to finish previous transactions
      }
    } }
  }

  private def sendOut(messagebuffer: Seq[Seq[(String, Msgi)]]): Unit = {
    val tmp = messagebuffer.flatten
    if (tmp.isEmpty) return
    concreteplatform.fpost("./push", write(tmp.map { case (a,b) => (a, write(b)) }))
  }

//  def bufferTime[X](incoming: Rx[X], beforeMillis: Int, betweenMillis: Int)
//                   (implicit readWriter: ReadWriter[X]): Var[Seq[X]] = {
//    val buffer = mutable.ListBuffer[X]()
//    var running = false
//    val outgoing = Var[Seq[X]](Seq[X]())
//
//    def f(): Unit = {
//      if (buffer.nonEmpty) {
//        val tmp = Seq() ++ buffer
//        buffer.clear()
//        outgoing set tmp
//        concreteplatform.after(betweenMillis)(f)
//      } else {
//        running = false
//      }
//    }
//
//    incoming map /* map vs foreach */{ x =>
//      buffer ++= Seq(x)
//      if (!running) {
//        running = true
//        concreteplatform.after(beforeMillis)(f)
//      }
//    } foreach (_ => ())
//
//    outgoing
//  }
//  bufferTime(outbox, 100, 100) map sendOut foreach (_ => ())

  //private var ignore = false
  private var running = false
  private def appendOutbox(to: Remote.ClientId, msg: Msgi): Unit = {
    outbox append ((to, msg))

//    def f(): Unit = {
//      if (outbox.nonEmpty) {
        val tmp = Seq() ++ outbox
        outbox.clear()
        println("send --> " + tmp)
        sendOut(Seq(tmp))
//        concreteplatform.after(100)(f)
//      } else {
//        running = false
//      }
//      }

//    if (!running) {
//      running = true
//      concreteplatform.after(100)(f)
//    }
  }

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

