package platform

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.concurrent.ThreadLocalRandom

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import drx.{AbstractWeakMap, AbstractWeakSet, ScalaWeakMap, abstractplatform}
import javafx.util.Duration
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.Platform

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** Created by david on 17.09.17. */
trait platform extends abstractplatform {
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  override implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Platform.runLater(_))
  override def fpost(url: String, body: String): Future[String] =
    Http().singleRequest(
        HttpRequest(HttpMethods.POST, "http://localhost:8080/" + url,
          entity = HttpEntity(ContentTypes.`application/json`, body.getBytes())))
    .flatMap[String](x => Unmarshal(x.entity).to[String])
  override def after[X](millis: Double)(func: () => X): Unit = {
    new Timeline(new KeyFrame(Duration.millis(millis), _ => func())).play()
  }

  override def heapSize(): Double = java.lang.Runtime.getRuntime.totalMemory() - java.lang.Runtime.getRuntime.freeMemory() * 1.0
  override def gc(): Unit = System.gc()

  override def WeakMap[K,V]() = new ScalaWeakMap[K,V]()
  override def WeakSetOrNot[K, V](): AbstractWeakSet[K, V] = new drx.WeakSet()
  //override def WeakSetOrNot[K, V](): AbstractWeakSet[K, V] = new drx.AllSet()
  //override def WeakSetOrNot[K, V](): AbstractWeakSet[K, V] = new drx.NoSet()


  // remove old outputs
  Files.createDirectories(Paths.get("debuggraphs"))
  new File("debuggraphs")
    .listFiles(f => f.isFile && (f.getName.endsWith(".svg") || f.getName.endsWith(".dot")))
    .foreach(f => f.delete())
  var j = ThreadLocalRandom.current().nextInt().toHexString.slice(0, 8)
  var i = 0
  override def writeToDisk(desc: String): Unit = {
    i += 1
    Files.write(Paths.get(s"debuggraphs/graph-$j-$i.dot"), drx.debug.stringit(desc=desc).getBytes())
  }

  override val measurements: mutable.ListBuffer[Double] = mutable.ListBuffer()
  override def startMeasure(): Unit = {}
  override def endMeasure(): Unit =   {}
}
