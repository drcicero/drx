/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

import java.util.concurrent.ThreadLocalRandom

import Network._
import RxDom._
import RxDomHelper.{rxInput, _}
import drx._
import org.scalajs.dom
import scalatags.JsDom.all._
import upickle.default.{ReadWriter, macroRW}

import scala.language.implicitConversions
import scala.scalajs.js.Date

object AppChat {
  case class LocalMsg(id: String, timestamp: Long, content: Var[String])
  case class Msg(id: String, timestamp: Long, content: Var[String], clientId: ClientId)
  implicit def localRW: ReadWriter[LocalMsg] = macroRW
  implicit def remoteRW: ReadWriter[Msg] = macroRW

  val localnick: Var[String] = Var("")
  val localHistory = new IncMap[LocalMsg]()
  def makeNewMessage(value: String): Unit = localHistory.update {
    val rnd = ThreadLocalRandom.current().nextInt().toString
    Seq(rnd -> LocalMsg(rnd, new java.util.Date().getTime, Var(value)))
  }

  def getAllOthers[X](sig: Rx[X], id: String, startup: () => X)
                     (implicit e: ReadWriter[X]): Rx[(ClientId, X)] = {
    Network.offer(sig, startup, id)
    Network.sub[X](sig.sample, id)
  }

  def main(): Unit = {

    val dcommonHistory: Rx[Seq[(String, Msg)]] = getAllOthers(localHistory.diffs, "history", ()=>localHistory.aggregate.get.toSeq)
      .map { case (clientId, lmsg) =>
        lmsg.map{ case (x,y) => x -> Msg(y.id, y.timestamp, y.content, clientId) }}
    val commonHistory: Rx[Map[String, Msg]] = dcommonHistory
      .scan(Map[String, Msg]()){ case (state, event) =>
        (state ++ event) filter { _._2 != null }
      }

    val nicks = getAllOthers(localnick, "nick", ()=>localnick.get).scan(Map[ClientId, String]())(
      (state, evt) => if (evt._2 != "") state + evt else state)

    Network.startHeartbeat()

    val obj = div(
      h1("CHAT"),
      rxInput(localnick, Val(placeholder:=Network.localId)),
      div(dcommonHistory.dmapmap{ msg =>
        if (dom.window.innerHeight + dom.window.pageYOffset + 10 > dom.document.documentElement.scrollHeight)
          scala.scalajs.js.timers.setTimeout(1) (dom.document.body.lastElementChild.lastElementChild.scrollIntoView())

        val d = new Date(msg.timestamp)
        div(
          span("%02d:%02d:%02d ".format(d.getHours(), d.getMinutes(), d.getSeconds()),
            nicks.map(nicks => span(nicks.getOrElse(msg.clientId, msg.clientId).asInstanceOf[String])), ": ",
            style:="display:inline-block;width:40%"),
          msg.content.map(span(_)))
      }),
      rxCommand(makeNewMessage, placeholder:="send message"),
    )
    replaceChild(dom.document.body, dom.document.body.lastElementChild, obj.render)

  }

}
