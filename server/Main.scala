package server

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
  var log = mutable.Map[String, mutable.ListBuffer[String]]()
  var waiting = mutable.Map[String, Promise[String]]()
  var cleanup = mutable.Map[String, Promise[String]]()

  def timeoutElse[X](x: Future[X], i: Long)(alt: () => X): Future[X] =
    Future.firstCompletedOf(Seq(x,
      Future { Thread.sleep(10000); alt() }))

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val route = {
      get {
        path("") { getFromFile("index.html") } ~
        path("viz.js") { getFromFile("viz.js") } ~
        path("todojs.js") { getFromFile {
          "/tmp/sbt/" + new java.io.File("").getAbsolutePath().replace("/", "-") + "-todojs/scala-2.12/todojs-fastopt.js"
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
                promise.success(write((log.keys.toSet, news)))
                waiting.remove(name)
              }
            }
          }
          complete("")
        } } ~
        path("pull" / Segment) { name =>
          this.synchronized {
            val newConnection = !log.contains(name)
            val news = log.getOrElseUpdate(name, mutable.ListBuffer()).toSeq; log(name).clear()
            if (newConnection || news.nonEmpty) {
              println("immediate {}")
              complete(write((log.keys.toSet, news)))
            } else {
              println("waiting {")
              val promise = Promise[String]()
              val thefuture = timeoutElse(promise.future, 5000) { () => this.synchronized {
                if (!waiting(name).isCompleted) {
                  println("} timeout")
                  val news = log.getOrElseUpdate(name, mutable.ListBuffer()).toSeq; log(name).clear()
                  write((log.keys.toSet, news))
                } else ""
              } }
              waiting += name -> promise
              complete(thefuture)
            }
          }
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
