object compat2 {

  def heapSize(): Double = {
    val process = scala.scalajs.js.Dynamic.global.process
    if (scalajs.js.isUndefined(process)) 0
    else process.memoryUsage().heapUsed.asInstanceOf[Double]
  }
  def gc(): Unit = scala.scalajs.js.Dynamic.global.gc()

// def PotentialWeakHashMap[K,V]() = new scala.collection.mutable.HashMap[K,V]()
  def PotentialWeakHashMap[K,V]() = new NoMap[K,V]()

//  //  type PotentialWeakHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
//  type PotentialWeakHashMap[K,V] = NoMap[K,V]

}