package server

import java.io
import java.util.concurrent.TimeUnit

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
import scala.concurrent.duration.Duration
import scala.io.StdIn

// thanks to https://github.com/lihaoyi/workbench-example-app/tree/autowire-akka-http

object Server {
  // ~5 min maximum for browser?
  private val LONGPOLL_DURATION_MIN = 4

  var active = mutable.Map[String, Long]()
  var log = mutable.Map[String, mutable.ListBuffer[String]]()
  var waiting = mutable.Map[String, Promise[String]]()

//  def timeoutElse[X](x: Future[X], i: Long)(alt: () => X): Future[X] =
//    Future.firstCompletedOf(Seq(x,
//      Future { Thread.sleep(i); alt() }))

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val route = {
      get {
        path("") { getFromFile("index.html") } ~
        path("viz.js") { getFromFile("viz.js") } ~
        path("todojs.js") { getFromFile {
          val patha = "/tmp/sbt/" + new java.io.File("").getAbsolutePath().replace("/", "-") + "-todojs/scala-2.12/todojs-fastopt.js"
          val pathb = "/tmp/sbt/" + new java.io.File("").getAbsolutePath().replace("/", "-") + "-todojs/scala-2.12/todojs-opt.js"
          if (new java.io.File(patha).lastModified() > new java.io.File(pathb).lastModified()) patha else pathb
        } } ~
        path("quit") { system.terminate(); complete{ "goto /index.html" } }
      } ~
      post {
        path("push") { entity(as[String]) { str =>
          this.synchronized {
            read[Seq[(String, String)]](str).foreach { case (client, news) => log.getOrElseUpdate(client, mutable.ListBuffer()) += news }
            waiting.foreach { case (name, promise) =>
              println("} news")
              val news = log.getOrElseUpdate(name, mutable.ListBuffer()).toSeq; log(name).clear()
              if (news.nonEmpty) {
                promise.success(mkResponse(news))
                waiting.remove(name)
              }
            }
          }
          complete("")
        } } ~
        withRequestTimeout(Duration(LONGPOLL_DURATION_MIN, TimeUnit.MINUTES)) { path("pull" / Segment) { name =>
          this.synchronized {
            active(name) = System.currentTimeMillis() // still alive

            val newConnection = !log.contains(name)
            val news = log.getOrElseUpdate(name, mutable.ListBuffer()).toSeq; log(name).clear()
            if (newConnection || news.nonEmpty) {
              println("immediate {}")
              complete(mkResponse(news))
            } else {
              println("waiting {")
              val promise = Promise[String]()
              waiting += name -> promise
              withRequestTimeoutResponse { request =>
                HttpResponse(StatusCodes.OK, entity=this.synchronized {
                  if (!waiting(name).isCompleted) {
                    println("} timeout")
                    val news = log.getOrElseUpdate(name, mutable.ListBuffer()).toSeq; log(name).clear()
                    mkResponse(news)
                  } else ""
                })
              } {
                complete(promise.future)
              }
            }
          }
        } }
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

  private def mkResponse(news: Seq[String]) = {
    val LONGPOLL_TOLERANCE_MIN = 1 // let server forget clients after 1min pause (~ 4+1 == 5min)
    val stillActive = System.currentTimeMillis() - 60 * 1000 * (LONGPOLL_DURATION_MIN + LONGPOLL_TOLERANCE_MIN)
    write((active.filter(x => x._2 > stillActive).keys, news))
  }
}
