/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

import java.util.concurrent.ThreadLocalRandom

import Network.{ClientID, fetchBase64, fetchText}
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
  case class Msg(id: String, timestamp: Long, content: Var[String], clientId: ClientID)
  implicit def localRW: ReadWriter[LocalMsg] = macroRW
  implicit def remoteRW: ReadWriter[Msg] = macroRW

  val localnick: Var[String] = new Var("", "nick")
  val localHistory = new IncMap[LocalMsg]("history")
  def makeNewMessage(value: String): Unit = localHistory.update {
    val rnd = ThreadLocalRandom.current().nextInt().toString
    Seq(rnd -> LocalMsg(rnd, new java.util.Date().getTime, new Var(value)))
  }

  def getAllOthers[X](sig: Var[X])
                     (implicit e: ReadWriter[X]): Rx[(ClientID, X)] = {
    Network.pub(sig)
    Network.sub[X](sig.sample, sig.id)
  }

  def main(): Unit = {

    val dcommonHistory: Rx[Seq[(String, Msg)]] = getAllOthers(localHistory.diffs)
      .map { case (clientId, lmsg) =>
        lmsg.map{ case (x,y) => x -> Msg(y.id, y.timestamp, y.content, clientId) }}
    val commonHistory: Rx[Map[String, Msg]] = dcommonHistory
      .scan(Map[String, Msg]()){ case (state, event) =>
        (state ++ event) filter { _._2 != null }
      }

    val nicks = getAllOthers(localnick).scan(Map[ClientID, String]())(
      (state, evt) => if (evt._2 != "") state + evt else state)

    Network.startHeartbeat()

    val obj = div(
      h1("CHAT"),
      rxInput(localnick, Val(placeholder:=Network.name.get)),
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
