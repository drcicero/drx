/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

import java.util.concurrent.ThreadLocalRandom

import drx.Remote._
import RxDom._
import RxDomHelper.{rxInput, _}
import drx._
import drx.graph.{Rx, Val, Var}
import org.scalajs.dom
import scalatags.JsDom.all._
import upickle.default.{ReadWriter, macroRW}

import scala.language.implicitConversions
import scala.scalajs.js.Date

object AppChat {
  case class Item(id: String, timestamp: Long, content: Var[String], clientId: ClientId)
  implicit def itemRW: ReadWriter[Item] = macroRW

  val localnick: Var[String] = Var("")
  val localHistory = new IncMap[Item]()
  def makeNewMessage(value: String): Unit = localHistory.update {
    val rnd = ThreadLocalRandom.current().nextInt().toString
    Seq(rnd -> Some(Item(rnd, new java.util.Date().getTime, Var(value), Remote.thisClient)))
  }

  def main(sync: Boolean): Unit = {

    val dcommonHistory: Rx[Seq[(String, Option[Item])]] =
      sharedAs(localHistory.diffs, "history", localHistory.sampleAsDelta _, sync=sync)
      .map { case (clientId, lmsg) => lmsg }
//    val dcommonHistory: Rx[Seq[(String, Option[Item])]] =
//      sharedAs(localHistory.aggregate, "history", ()=>localHistory.aggregate.get)
//      .map { case (clientId, lmsg) => lmsg }.toSeq
//    val commonHistory: Rx[Map[String, Item]] = dcommonHistory
//        .scan(Map[String, Item]())(IncMap.add[Item])

    val nicks = sharedAs(localnick, "nick", ()=>localnick.get, sync=sync)
      .scan(Map[ClientId, String]())(
        (state, evt) => if (evt._2 != "") state + evt else state)

    Remote.startHeartbeat()

    val obj = div(
      h1("CHAT " + (if (sync) "SYNC" else "ASYNC")),
      rxInput(localnick, Val(placeholder:=Remote.thisClient)),
      div(dcommonHistory.dmapmap { msg =>
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
