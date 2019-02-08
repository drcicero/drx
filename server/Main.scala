package example

import java.io

import upickle.default.write
import upickle.default.read
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.collection.mutable
import scala.io.StdIn

// thanks to https://github.com/lihaoyi/workbench-example-app/tree/autowire-akka-http

object Server {
  var join = 0
  var log = mutable.ListBuffer[String]()
  var waiting: Seq[Promise[String]] = Seq[Promise[String]]()

  def timeout[X](x: Future[X], i: Long): Future[X] =
    Future.firstCompletedOf(Seq(x,
      Future { Thread.sleep(10000); sys.error("changes!") }))

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val route = {
      get {
        path("quit") { system.terminate(); complete{ "goto /index.html" } } ~
        getFromBrowseableDirectory("..")
      } ~
      post {
        path("join") { join.synchronized{ join+=1; complete("bot" + join) } } ~
        path("push") { entity(as[String]) { str =>
          val newseq = read[Seq[String]](str)
          log.synchronized(log ++= newseq)
          waiting.synchronized {
            waiting.foreach(x => x.failure(new RuntimeException("changes!")))
            waiting = Seq()
          }
          complete("")
        } } ~
        path("pull" / Segment) { watermark =>
          val (newWatermark, news) = log.synchronized(
            (log.length, log.drop(watermark.toInt).toSeq))

          val promise = Promise[String]()
          val thefuture = timeout(promise.future, 1000).recover {
            case _ =>
              println("} timeout / new")
              val (loglength, value) = log.synchronized(
                (log.length, log.drop(watermark.toInt).toSeq))
              write((loglength, value))
          }

          waiting.synchronized {
            if (news.isEmpty) {
              waiting ++= Seq(promise)
              println("waiting {")
            } else {
              promise.success(write((newWatermark, news)))
              println("immediate {}")
            }
          }

          complete(thefuture)
        }
      }
    }

    println("serve from " + System.getProperty("user.dir") + "/../")
    println("      to   http://localhost:8080")
    Http().bindAndHandle(route, "localhost", port = 8080)

    println("readline: ")
    val x = StdIn.readLine()
    println(x)
    system.terminate()
  }
}