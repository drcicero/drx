package drx

import scala.collection.mutable

/** Created by david on 10.06.17. */
object debug {

  /** only for ghost testing / debugging variables */
  class NoMap[K, V] {
    def update(a: K, b: V): Unit = Unit
    def map[R](x: (K, V) => R): List[R] = List.empty
    def keys: List[K] = List.empty
    def size = 0
  }

  private[drx] val debugRxs = compat2.compat2.PotentialWeakHashMap[Node[_], Unit]()

  def printless(): Unit = {
    println(compat2.compat2.heapSize())
//    println("v=" + debugVars.size + " s=" + debugSigs.size +
//      " o=" + debugObs.size + " heap=" + compat2.heapSize() / 10000000)
  }

  def doit() = {
    printless()
    for (i <- 0 to 5) compat2.compat2.gc()
    printless()
  }

  def serialize(x: Any): String = x match {
    case None       => "none"
    case Some(y)    => serialize(y)
    case x: Node[_] => x.id
    // case x: Node => x.outerHTML.replace('"', "'")
    case x: mutable.Map[_, _]      =>
      "{" + x.map {
        case (a, b) => "" + serialize(a) + ":" + serialize(b)
      }.mkString(", ") + "}"
    case x: mutable.Traversable[_] =>
      "[" + x.map(it => serialize(it)).mkString(", ") + "]"
    case _ => "" + x
  }

  def getthem(rx: Node[_], acc: mutable.Set[Node[_]]): Unit = if (!acc.contains(rx)) {
    acc += rx
    rx.ins.foreach { y => getthem(y, acc) }
    rx.getOuts.foreach { y => getthem(y, acc) }
  }

  def transitivehull(xs: Set[Node[_]]): Set[Node[_]] = {
    val set = mutable.Set[Node[_]]()
    xs.foreach(getthem(_, set))
    set.toSet
  }

//  def transitivehullobservers(xs: Set[Observer[_]]): Set[Observer[_]] = {
//    val set = mutable.Set[DataflowNode[_]]()
//    xs.flatMap(_.getOuts).foreach(getthem(_, set))
//    set.flatMap(_.getOuts).collect { case (x:Observer[_]) => x }.toSet
//  }

  def stringit(root: Set[Node[_]] = Set()): String = {

    val sigs = transitivehull(root ++ debugRxs.keys)

    def color(rx: Node[_]): String = rx match {
      case _: Var[_]      => "#00aadd"
      case _: Token       => "#ddaa00"
      case it: Reactor[_] => if (it.calcActive) "#aadd00" else "silver"
    }

    ("digraph {\n" +

      sigs.map {it => (

        s"""  "${it.id}" [style=filled,fillcolor="${color(it)}",tooltip="${serialize(it.value)}"]\n""" +

          it.ins.filter(!_.getOuts.toSet.contains(it)).map {
            dep => s"""  "${dep.id }" -> "${it.id }" [color=silver dir=back]"""
          }.mkString("\n") +

          it.getOuts.map {
            child => s"""  "${it.id }" -> "${child.id }" [dir=both]"""
          }.mkString("\n") +

          "\n"

        )}.mkString("\n") + "\n" +

      "\n}\n\n")
  }
}
