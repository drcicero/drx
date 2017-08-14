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

}