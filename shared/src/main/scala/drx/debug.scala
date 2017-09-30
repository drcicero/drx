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

  private[drx] val debugRxs = compat2.compat2.PotentialWeakHashMap[GraphNode[_], Unit]()

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
    case None               => "none"
    case Some(y)            => serialize(y)
    case x: GraphNode[_] => x.id
    // case x: Node => x.outerHTML.replace('"', "'")
    case x: mutable.Map[_, _]      =>
      "{" + x.map {
        case (a, b) => "" + serialize(a) + ":" + serialize(b)
      }.mkString(", ") + "}"
    case x: mutable.Traversable[_] =>
      "[" + x.map(it => serialize(it)).mkString(", ") + "]"
    case _ => "" + x
  }

  def getthem(rx: GraphNode[_], acc: mutable.Set[GraphNode[_]]): Unit = if (!acc.contains(rx)) {
    acc += rx
    rx.ins.foreach { y => getthem(y, acc) }
    rx.getOuts.foreach { y => getthem(y, acc) }
  }

  def transitivehull(xs: Set[GraphNode[_]]): Set[GraphNode[_]] = {
    val set = mutable.Set[GraphNode[_]]()
    xs.foreach(getthem(_, set))
    set.toSet
  }

//  def transitivehullobservers(xs: Set[Observer[_]]): Set[Observer[_]] = {
//    val set = mutable.Set[DataflowNode[_]]()
//    xs.flatMap(_.getOuts).foreach(getthem(_, set))
//    set.flatMap(_.getOuts).collect { case (x:Observer[_]) => x }.toSet
//  }

  def stringit(root: Set[GraphNode[_]] = Set()): String = {

    val sigs = transitivehull(root ++ debugRxs.keys)

    def color(rx: GraphNode[_]): String = (rx match {
      case _: EventSource[_]   => "fillcolor=\"#00aadd\",shape=invtriangle"
      case it: Callback[_]     => "fillcolor=\"" + (if (it.isNeeded) "#ddaa00" else "silver") +"\",shape=triangle"
      case it: DerivedValue[_] => if (!it.isNeeded) "fillcolor=silver,shape=diamond"
      else if (it.getOuts.isEmpty) "fillcolor=\"red\",shape=diamond" else "fillcolor=\"#aadd00\",shape=diamond"
    }) + ",color=\"" + (if(rx.frozen) "blue" else "white") + "\""

    ("digraph {\n" +

      sigs.map {it => (

        s"""  "${it.id+"\\n"+it.level}" [style=filled,${color(it)},tooltip="${serialize(it.value)}",fixedsize=shape]\n""" +

          (it match {
            case it: DerivedValue[_] =>
              it.ins
                .filter { dep => !dep.getOuts.contains(it) }
                .map { dep =>
                s"""  "${dep.id+"\\n"+dep.level}" -> "${it.id+"\\n"+it.level}" [color=silver dir=back]"""
              }.mkString("\n")
            case _ => ""
          }) +

          it.getOuts.map { child =>
            s"""  "${it.id+"\\n"+it.level}" -> "${child.id+"\\n"+child.level}" [dir=both]"""
          }.mkString("\n") +

          (it match {
            case it: Store[_,_] =>
              it.now.flatMap(_.getVariables).map { child =>
                s"""  "${it.id+"\\n"+it.level}" -> "${child.id+"\\n"+child.level}" [color=aqua]"""
              }.mkString("\n")
            case _ => ""
          })+

          "\n"

        )}.mkString("\n") + "\n" +

      "\n}\n\n")
  }
}
