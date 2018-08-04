package example
import upickle.default._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContext.Implicits.global

// thanks to https://github.com/lihaoyi/workbench-example-app/tree/autowire-akka-http

object Server {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    val route = {
      get {
        path("quit") { system.terminate(); complete{"shutdown"} } ~
//        pathSingleSlash { complete { HttpEntity(ContentTypes.`text/html(UTF-8)`, Template.txt) } } ~
        getFromBrowseableDirectory("..")
      } ~
      post {
        path("api" / Segments){ s =>
          println("hey " + s)
          Thread.sleep(1000)
          complete{ write(List(1,2,3)) }
        }
      }
    }
    println("serve from " + System.getProperty("user.dir") + "/../")
    println("      to   http://localhost:8080")
    Http().bindAndHandle(route, "localhost", port = 8080)
  }
}
