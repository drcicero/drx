package platform

import scala.scalajs.js

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

  def writeToDisk(): Unit = {}
}
