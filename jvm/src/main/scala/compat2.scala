object compat2 {

  def heapSize() = java.lang.Runtime.getRuntime().totalMemory() - java.lang.Runtime.getRuntime().freeMemory() * 1.0
  def gc() = System.gc()

  def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.WeakHashMap[K,V]()
//  def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.HashMap[K,V]()
//  def PotentialWeakHashMap[K,V]() = new NoMap[K,V]()

//  //  type PotentialWeakHashMap[K,V] = scala.collection.mutable.WeakHashMap[K,V]
//  type PotentialWeakHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
////  type PotentialWeakHashMap[K,V] = NoMap[K,V]

}