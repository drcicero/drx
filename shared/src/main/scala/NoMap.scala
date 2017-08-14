/** Created by david on 28.07.17. */

// ghost testing / debugging variables
class NoMap[K, V] {
  def update(a: K, b: V): Unit = Unit
  def map[R](x: (K, V) => R): List[R] = List.empty
  def keys: List[K] = List.empty
  def size = 0
}
