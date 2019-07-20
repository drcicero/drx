package platform

import drx.{AbstractWeakMap, AbstractWeakSet, abstractplatform}
import org.scalajs.dom.experimental.{Fetch, HttpMethod, RequestInit}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

@js.native @js.annotation.JSGlobal("WeakMap")
class RawJSWeakMap[Key, Value] extends js.Object {
  def delete(key: Key): Unit = js.native
  def has(key: Key): Boolean = js.native
  def get(key: Key): js.UndefOr[Value] = js.native
  def set(key: Key, value: Value): Unit = js.native
}
class JSWeakMap[Key, Value] extends AbstractWeakMap[Key, Value] {
  private val map = new RawJSWeakMap[Key, Value]()
  override def delete(key: Key): Unit = map.delete(key)
  override def has(key: Key): Boolean = map.has(key)
  override def get(key: Key): Option[Value] = map.get(key).toOption
  override def set(key: Key, value: Value): Unit = map.set(key, value)
}

/** Created by david on 17.09.17. */
//  type PotentialWeakHashMap[K,V] = NoMap[K,V]
trait platform extends abstractplatform {
  override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  override def fpost(url: String, body: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.POST, body = body))
      .toFuture.flatMap(x => x.text.toFuture)
  override def after[X](millis: Double)(func: () => X): Unit = setTimeout(millis){ func() }

  override def heapSize(): Double = js.Dynamic.global.process.memoryUsage().heapUsed.asInstanceOf[Double]
  override def gc(): Unit = try { js.Dynamic.global.gc() } catch { case _: Exception => }

  override def WeakMap[K,V]() = new JSWeakMap[K,V]()
  //override def WeakSetOrNot[K, V](): AbstractWeakSet[K, V] = new drx.AllSet()
  override def WeakSetOrNot[K, V](): AbstractWeakSet[K, V] = new drx.NoSet()

  private var isMeasuring: Boolean = false
  override def writeToDisk(desc:String=""): Unit = {}
  override def startMeasure(): Unit = if (!isMeasuring) {
    isMeasuring = true
    measurements += js.Date.now() //scala.scalajs.js.Performance.now()
  }
  override def endMeasure(): Unit = if (isMeasuring) {
    isMeasuring = false
    measurements += js.Date.now() //scala.scalajs.js.Performance.now()
  }
}
