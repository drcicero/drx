package platform

import drx.{Obs, Rx}
import org.scalajs.dom

import scala.scalajs.js
import scala.collection.mutable

/** Created by david on 17.09.17. */
object platform {

  def heapSize(): Double = js.Dynamic.global.process.memoryUsage().heapUsed.asInstanceOf[Double]
  def gc(): Unit = js.Dynamic.global.gc()

// def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.HashMap[K,V]()
  def PotentialWeakHashMap[K,V]() = new drx.debug.NoMap[K,V]()

//  //  type PotentialWeakHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
//  type PotentialWeakHashMap[K,V] = NoMap[K,V]

  @js.native @js.annotation.JSGlobal("WeakMap")
  class WeakMap[Key, Value] extends js.Object {
    def delete(key: Key): Unit = js.native
    def has(key: Key): Boolean = js.native
    def get(key: Key): js.UndefOr[Value] = js.native
    def set(key: Key, value: Value): Unit = js.native
  }

  def writeToDisk(desc:String=""): Unit = {}

  val measurements: mutable.ListBuffer[Double] = mutable.ListBuffer()
  def startMeasure(): Unit = measurements += js.Date.now() //scala.scalajs.js.Performance.now()
  def endMeasure(): Unit =   measurements += js.Date.now() //scala.scalajs.js.Performance.now()
}
