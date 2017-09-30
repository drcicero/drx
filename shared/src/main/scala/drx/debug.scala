package drx

import scala.collection.mutable
import scala.util.{Failure, Success}

/** Created by david on 10.06.17. */
object debug {
  /** only for ghost testing / debugging variables */
  class NoMap[K, V] {
    def update(a: K, b: V): Unit = Unit
    def map[R](x: (K, V) => R): List[R] = List.empty
    def keys: List[K] = List.empty
    def size = 0
  }

  def serialize(x: Any): (String, String) = {
    val tmp = x match {
      case Failure(e) if e.isInstanceOf[EmptyStream] => "/"
      case Success(y)           => serialize(y)._2
      case x: InternalRx[_]     => x.id
      // case x: Node => x.outerHTML.replace('"', "'")
      case x: mutable.Map[_, _]      =>
        "{" + x.map {
          case (a, b) => "" + serialize(a)._2 + ":" + serialize(b)._2
        }.mkString(", ") + "}"
      case x: mutable.Traversable[_] =>
        "[" + x.map(it => serialize(it)._2).mkString(", ") + "]"
      case _ => ("" + x).replaceAllLiterally("\"", "\\\"")
    }
    if (tmp.length < 10) (tmp,tmp)
    else (tmp.slice(0,7) + "...",tmp)
  }

  def getthem(rx: InternalRx[_], acc: mutable.Set[InternalRx[_]]): Unit = if (!acc.contains(rx)) {
    acc += rx
    rx.getIns.foreach { y => getthem(y, acc) }
    rx.getOuts.foreach { y => getthem(y, acc) }
  }

  def transitivehull(xs: Set[InternalRx[_]]): Set[InternalRx[_]] = {
    val set = mutable.Set[InternalRx[_]]()
    xs.foreach(getthem(_, set))
    set.toSet
  }

//  def transitivehullobservers(xs: Set[Observer[_]]): Set[Observer[_]] = {
//    val set = mutable.Set[DataflowNode[_]]()
//    xs.flatMap(_.getOuts).foreach(getthem(_, set))
//    set.flatMap(_.getOuts).collect { case (x:Observer[_]) => x }.toSet
//  }

  def stringit(root: Set[_ <: InternalRx[_]] = Set()): String = {

    val sigs = transitivehull(root ++ debugRxs.keys)

    def color(rx: InternalRx[_]): String =
      "color=" + (
        if (rx.forceActive) "black" // "\"#ddaa00\""
        else if (rx.isActive) "black" // "\"#aadd00\""
        else "white"
      ) +
      ",fillcolor=" + (
        if (rx.debugEvaluating) "\"#ddaa00\"" // orange
//        else if (withTransaction.activeCtx.value.exists(_.ready.contains(rx))) "\"#dddd00\"" // yellow
        else if (withTransaction.activeCtx.value.exists(_.dirty.contains(rx))) "\"#aadd00\"" // green
        else if (withTransaction.activeCtx.value.exists(_.clean.contains(rx))) "\"silver\""
//        else if (rx.isFrozen) "\"#00ddaa\"" // "silver"
        else "white")
//      + "," + (rx match {
//        case _: EventSource[_] => "shape=triangle"
//        case _: Sink[_]        => "shape=invtriangle"
//        case _                 => "shape=diamond"
//      })

    def str(rx: InternalRx[_]): String = rx.hashCode().toString

      // "\"" + rx.id +"\\n"+ serialize(rx.value) + "\"" // "\"%s\\n%d\"".format(rx.id, rx.level)

    val ranks = sigs.groupBy(x => x.level)

    ("digraph {\n" +

      ranks.map(x => "{ rank=same %s %s }\n".format(
        x._1, x._2.map(str).mkString(" ")
      )).mkString("") +

      ranks.map(x => s"${x._1} [shape=none]\n").mkString("") +
      ranks.keys.toList.sorted.mkString(" -> ") + " [arrowhead=none]\n" +

      sigs.map {it => (

        "  %s [%s,label=\"%s\",tooltip=\"%s\",shape=rectangle,style=filled,fixedsize=shape]\n"
          .format(str(it), color(it), it.id +"\\n"+ serialize(it.value)._1, serialize(it.value)._2) +

          it.getIns.filter { father => !father.getOuts.contains(it) }
            .map { dep =>
              "  %s -> %s [color=silver dir=back]".format(str(dep), str(it))
            }.mkString("\n") +

          it.getOuts.map { child =>
            "  %s -> %s [dir=both]".format(str(it), str(child))
          }.mkString("\n") +

//          (it match {
//            case it: Store[_,_] => // oops
//              it.getEventsources.map { child =>
//                "  %s -> %s [color=aqua]".format(str(it), str(child.underlying))
//              }.mkString("\n")
//            case _ => ""
//          })+

          "\n"

        )}.mkString("\n") + "\n" +

      "\n}\n\n")
  }

  //////////////////////////////////////////////////////////////////////////////
  // platform specific

  private[drx] val debugRxs = platform.platform.PotentialWeakHashMap[InternalRx[_], Unit]()

  def writeToDisk(): Unit = platform.platform.writeToDisk()

  def printless(): Unit = {
    println(platform.platform.heapSize())
//    println("v=" + debugVars.size + " s=" + debugSigs.size +
//      " o=" + debugObs.size + " heap=" + compat2.heapSize() / 10000000)
  }

  def doit(): Unit = {
    printless()
    for (i <- 0 to 5) platform.platform.gc()
    printless()
  }
}
