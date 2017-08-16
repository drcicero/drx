package compat2

object compat2 {

  def heapSize(): Double = java.lang.Runtime.getRuntime().totalMemory() - java.lang.Runtime.getRuntime().freeMemory() * 1.0
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

}