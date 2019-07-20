///** Created by david on 05.05.17. */
//
//// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?
//
//import java.util.concurrent.ThreadLocalRandom
//
//import drx.Network._
//import drx._
//import javafx.application.Application
//import upickle.default.{ReadWriter, macroRW}
//
//import scala.language.implicitConversions
//
//object AppChat extends Application {
//  case class LocalMsg(id: String, timestamp: Long, content: Var[String])
//  case class Msg(id: String, timestamp: Long, content: Var[String], clientId: ClientId)
//  implicit def localRW: ReadWriter[LocalMsg] = macroRW
//  implicit def remoteRW: ReadWriter[Msg] = macroRW
//
//  val localnick: Var[String] = Var("")
//  val localHistory = new IncMap[LocalMsg]()
//  def makeNewMessage(value: String): Unit = localHistory.update {
//    val rnd = ThreadLocalRandom.current().nextInt().toString
//    Seq(rnd -> LocalMsg(rnd, new java.util.Date().getTime, Var(value)))
//  }
//
//  def sharedAs[X](sig: Rx[X], id: String, startup: () => X)
//                     (implicit e: ReadWriter[X]): Rx[(ClientId, X)] = {
//    Network.offer(sig, startup, id)
//    Network.sub[X](sig.sample, id, false)
//  }
//
//  def start(): Unit = {
//
//    val dcommonHistory: Rx[Seq[(String, Msg)]] = sharedAs(localHistory.diffs, "history", ()=>localHistory.aggregate.get.toSeq)
//      //    val dcommonHistory: Rx[Seq[(String, Msg)]] = sharedAs(localHistory.aggregate, "history", ()=>localHistory.aggregate.get)
//      .map { case (clientId, lmsg) =>
//      lmsg.toSeq.map{ case (x,y) => x -> Msg(y.id, y.timestamp, y.content, clientId) }}
//    val commonHistory: Rx[Map[String, Msg]] = dcommonHistory
//      .scan(Map[String, Msg]()){ case (state, event) =>
//        (state ++ event) filter { _._2 != null }
//      }
//
//    val nicks = sharedAs(localnick, "nick", ()=>localnick.get).scan(Map[ClientId, String]())(
//      (state, evt) => if (evt._2 != "") state + evt else state)
//
//    Network.startHeartbeat()
//
//    val obj = div(
//      h1("CHAT"),
//      rxInput(localnick, Val(placeholder:=Network.thisClient)),
//      div(dcommonHistory.dmapmap{ msg =>
//        if (dom.window.innerHeight + dom.window.pageYOffset + 10 > dom.document.documentElement.scrollHeight)
//          scala.scalajs.js.timers.setTimeout(1) (dom.document.body.lastElementChild.lastElementChild.scrollIntoView())
//
//        val d = new Date(msg.timestamp)
//        div(
//          span("%02d:%02d:%02d ".format(d.getHours(), d.getMinutes(), d.getSeconds()),
//            nicks.map(nicks => span(nicks.getOrElse(msg.clientId, msg.clientId).asInstanceOf[String])), ": ",
//            style:="display:inline-block;width:40%"),
//          msg.content.map(span(_)))
//      }),
//      rxCommand(makeNewMessage, placeholder:="send message"),
//    )
//    replaceChild(dom.document.body, dom.document.body.lastElementChild, obj.render)
//
//  }
//
//}
