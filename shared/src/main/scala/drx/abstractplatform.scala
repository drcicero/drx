package drx

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

trait AbstractWeakSet[K, V] {
  def update(key: K, value: V): Unit
  def keys: Iterable[K]
}
/** only for ghost testing / debugging variables */
class NoSet[K, V] extends AbstractWeakSet[K, V] {
  override def update(a: K, b: V): Unit = Unit
  override def keys: Iterable[K] = List.empty
}
class AllSet[K, V] extends AbstractWeakSet[K, V] {
  private val map = scala.collection.mutable.HashMap[K, V]()
  override def update(a: K, b: V): Unit = map.update(a, b)
  override def keys: Iterable[K] = map.keys
}
class WeakSet[K, V] extends AbstractWeakSet[K, V] {
  private val map = scala.collection.mutable.WeakHashMap[K, V]()
  override def update(a: K, b: V): Unit = map.update(a, b)
  override def keys: Iterable[K] = map.keys
}

trait AbstractWeakMap[Key, Value] {
  def delete(key: Key): Unit
  def has(key: Key): Boolean
  def get(key: Key): Option[Value]
  def set(key: Key, value: Value): Unit
}
class ScalaWeakMap[Key, Value] extends AbstractWeakMap[Key, Value] {
  private val map = scala.collection.mutable.WeakHashMap[Key, Value]()
  override def delete(key: Key): Unit = map.remove(key)
  override def has(key: Key): Boolean = map.contains(key)
  override def get(key: Key): Option[Value] = map.get(key)
  override def set(key: Key, value: Value): Unit = map.update(key, value)
}

trait abstractplatform {
  def fpost(url: String, body: String): Future[String]
  def after[X](millis: Double)(func: () => X): Unit

  def heapSize(): Double
  def gc(): Unit

  val measurements: mutable.ListBuffer[Double] = mutable.ListBuffer()
  def startMeasure(): Unit
  def endMeasure(): Unit

  implicit val executionContext: ExecutionContext
  def WeakMap[K,V](): AbstractWeakMap[K,V]
  def WeakSetOrNot[K,V](): AbstractWeakSet[K,V]
  def writeToDisk(desc: String): Unit
}

object concreteplatform extends platform.platform with abstractplatform