import drx._
import upickle.default.{read, write, ReadWriter}

import scala.collection.mutable
import org.scalajs.dom.experimental.{Fetch, HttpMethod, RequestInit, Response}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.Uint8Array
import upickle.default.macroRW


object Network {
  type ClientID = String

  var name: Var[ClientID] = new Var("")
  fpost("./join", "abc").foreach(name set _)

  /* publish a variable */
  def pub(vari: Var[_]): Unit = {
    if (!published.add(vari)) return;
    //Var.incoming(vari.id) = vari
    println("publish "+vari.id)
    val obs = vari.strobserve(enqueueMsg(vari.id, _, _))
    // TODO timeout delete obs
  }

  /* subscribe to a variable name & type */
  def sub[X](init: X, variname: String)
            (implicit readwriter: ReadWriter[X]): Rx[(ClientID, X)] = {
    val vari = new Var[(String, X)](("", init))
    subscriptions += variname -> vari.asInstanceOf[Var[(ClientID, _)]]
    vari
  }

  def startHeartbeat(): Unit = heartbeat().foreach(_ =>
      scala.scalajs.js.timers.setTimeout(500)(startHeartbeat()))

  def fetchText(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.text.toFuture)

  // https://stackoverflow.com/questions/9267899/arraybuffer-to-base64-encoded-string/11562550#11562550
  def fetchBase64(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.arrayBuffer.toFuture).map{ x =>
      val btoa = org.scalajs.dom.window.btoa _
      val fromCharCode = scalajs.js.Dynamic.global.String.fromCharCode
      btoa(fromCharCode.applyDynamic("apply")(
        (), new Uint8Array(x)).asInstanceOf[String]) }

  def fpost(url: String, body: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.POST, body=body))
      .toFuture.flatMap(x => x.text.toFuture)

  /* --- private --- */

  private def sendOut(messagebuffer: Seq[(String, String)]): Unit = {
    if (messagebuffer.isEmpty) return
    val myname = name.sample
    val payload = write(messagebuffer map (msg => write((myname, msg))))
    fpost("./push", payload)
  }

  private def heartbeat(): Future[Unit] = {
    val myname = name.sample
    fpost("./pull/"+ watermark, "").map { blob =>
      val (newWatermark, news) = read[(Int, Seq[String])](blob)
      watermark = newWatermark
      news foreach { item =>
        val (source, (variId, message)) = read[(String, (String, String))](item)

        println(s"  recv $message -> $variId")
        if (source != myname) Var.incoming.get(variId).foreach(s => s set read(message)(s.rw).asInstanceOf) // update remote vars
        subscriptions.get(variId).foreach(_ parse "["+write(source)+", "+message+"]") // fullfil subscriptions
      }
    }
  }

  def bufferTime[X](incoming: Rx[X], i: Int)
                   (implicit readWriter: ReadWriter[X]) = {
    val buffer = mutable.ListBuffer[X]()
    incoming observe (x => buffer ++= Seq(x))

    val outgoing = new Var[Seq[X]](Seq[X]())
    def f(): Unit = scala.scalajs.js.timers.setTimeout(i) {
      val tmp = Seq() ++ buffer
      buffer.clear()
      outgoing set tmp
      f()
    }
    f()
    outgoing
  }

  //private var ignore = false
  private val outbox = new Var[(String, String)](("", ""))
  private var watermark = 0
  private val published = mutable.Set[Var[_]]()
  private val subscriptions = mutable.Map[String, Var[(ClientID, _)]]()

  bufferTime(outbox, 1000) observe sendOut

  private def enqueueMsg(id: String, embeddedVaris: Seq[Var[_]], msg: String): Unit = {
    embeddedVaris foreach pub // publish all embedded varis
    //if (ignore) return
    outbox set (id, msg)
    println(s"send ${id -> msg}")
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

