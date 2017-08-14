package compat2

object compat2 {

  def heapSize(): Double = scalajs.js.Dynamic.global.process.memoryUsage().heapUsed.asInstanceOf[Double]
  def gc(): Unit = scalajs.js.Dynamic.global.gc()

// def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.HashMap[K,V]()
  def PotentialWeakHashMap[K,V]() = new drx.debug.NoMap[K,V]()

//  //  type PotentialWeakHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
//  type PotentialWeakHashMap[K,V] = NoMap[K,V]

}