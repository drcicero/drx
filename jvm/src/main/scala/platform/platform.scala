package platform

import java.io.File
import java.nio.file.{Files, Paths}

/** Created by david on 17.09.17. */
object platform {

  def heapSize(): Double = java.lang.Runtime.getRuntime.totalMemory() - java.lang.Runtime.getRuntime.freeMemory() * 1.0
  def gc(): Unit = System.gc()

  def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.WeakHashMap[K,V]()
//  def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.HashMap[K,V]()
//  def PotentialWeakHashMap[K,V]() = new drx.debug.NoMap[K,V]()

//  //  type PotentialWeakHashMap[K,V] = scala.collection.mutable.WeakHashMap[K,V]
//  type PotentialWeakHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
////  type PotentialWeakHashMap[K,V] = new drx.debug.NoMap[K,V]

  class WeakMap[Key, Value] {
    private val map = scala.collection.mutable.WeakHashMap[Key, Value]()
    def delete(key: Key): Unit = map.remove(key)
    def has(key: Key): Boolean = map.contains(key)
    def get(key: Key): Option[Value] = map.get(key)
    def set(key: Key, value: Value): Unit = map.update(key, value)
  }

  // remove old outputs
  Files.createDirectories(Paths.get("debuggraphs"))
  new File("debuggraphs")
    .listFiles(f => f.isFile && (f.getName.endsWith(".svg") || f.getName.endsWith(".dot")))
    .foreach(f => f.delete())
  var i = 0
  def writeToDisk(): Unit = {
    i += 1
    Files.write(Paths.get(s"debuggraphs/graph$i.dot"), drx.debug.stringit().getBytes())
  }
}
