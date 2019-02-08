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

  def ellipsis(tmp: String): String =
    if (tmp.length < 10) tmp
    else tmp.slice(0,7) + "..."

  def serialize(x: Any): String = x match {
    case Failure(e) if e.isInstanceOf[EmptyValExc] => "/"
    case Success(y)           => serialize(y)
    case x: InternalRx[_]     => x.id
    // case x: Node => x.outerHTML.replace('"', "'")
    case x: mutable.Map[_, _]      =>
      "{" + x.map {
        case (a, b) => "" + serialize(a) + ":" + serialize(b)
      }.mkString(", ") + "}"
    case x: mutable.Traversable[_] =>
      "[" + x.map(it => serialize(it)).mkString(", ") + "]"
    case _ => ("" + x).replaceAllLiterally("\"", "\\\"")
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

  def stringit(root: Set[_ <: InternalRx[_]] = Set(), desc: String = ""): String = {

    def color(rx: InternalRx[_]): String =
      "color=" + (
        if (rx.getMode==Pushing) "black" // "\"#aadd00\""
        else "white"
      ) +
      ",fillcolor=" + (
        if (rx.shouldPush) "\"orange\""
        else if (rx.debugEvaluating) "\"#ddaa00\"" // orange
//        else if (withTransaction.activeCtx.value.exists(_.ready.contains(rx))) "\"#dddd00\"" // yellow
        else if (withInstant.activeCtx.value.exists(x =>
          x.pastDirtySources.contains(rx) || x.dirty.contains(rx))) "\"#aadd00\"" // green
        else if (withInstant.activeCtx.value.exists(_.clean.contains(rx))) "\"gray\""
        else if (rx.getMode==Frozen) "\"#00ddaa\"" // "gray"
        else "white")
//      + "," + (rx match {
//        case _: Var[_]  => "shape=triangle"
//        case _: Sink[_] => "shape=invtriangle"
//        case _          => "shape=diamond"
//      })

    def form(rx: InternalRx[_]): String = if (rx.id.contains("_")) rx.id.split("_",2)(1) else rx.id // rx.hashCode().toString
//    def str(rx: InternalRx[_]): String = '"'+rx.id.split("_",2)(1)+"_"+rx.level+'"'
    def str(rx: InternalRx[_]): String = rx.hashCode().toString

    def mapIt(kv: (String, collection.immutable.Set[InternalRx[_]])) = {
      val (k,l) = kv
      def intime(f: Instant => mutable.Set[InternalRx[_]])(it: InternalRx[_]) =
        withInstant.activeCtx.value.exists(f(_) contains it)
      (k, (
        "" + l.size + "x " + form(l.head) +" ("+ l.map(_.debugCtr).sum + ")",
        ellipsis(l map (it => serialize(it.value)) mkString ", "),
        l map (it => serialize(it.value)) mkString "\n",
        l flatMap (it => it.getIns filter (father => !(father.getOuts contains it))),
        l flatMap (_.getOuts),
        "fillcolor="+(
          if (l exists intime(_.pastDirtySources)) "\"#aadd00\""
          else if (l.exists(_.shouldPush)) "ivory3"
          else if (l exists intime(_.clean)) "ivory2"
          else "white")+
        ",color="+(
          if (l exists (x => x.value.isSuccess)) "black"
          else if (l exists (x => x.value.failed.get.isInstanceOf[EmptyValExc])) "none"
          else "red")
      ))
    }

    val sigs = transitivehull(root ++ debugRxs.keys)
    val gsigs = sigs.groupBy(str).map(mapIt)

      // "\"" + rx.id +"\\n"+ serialize(rx.value) + "\"" // "\"%s\\n%d\"".format(rx.id, rx.level)

    val ranks = sigs.groupBy(x => x.level)

    ("digraph {\n\n" +

      "graph [ fontsize=8 ]\n" +
      "edge [ fontsize=8 ]\n" +
      "node [ fontsize=8 ]\n" +
      "labelloc=\"t\";\n" +
      "label=\""+ desc.replace("\"", "'") +"\"; \n\n" +

      ranks.map(x => "{ rank=same %s %s }\n".format(
        x._1, x._2.map(str).mkString(" ")
      )).mkString("") +

      ranks.map(x => s"${x._1} [shape=none]\n").mkString("") +
      ranks.keys.toList.sorted.mkString(" -> ") + " [arrowhead=none]\n" +


      gsigs.map {case (k,(label, value1, value2, ins, outs, color)) => (

        "  %s [%s,label=\"%s\",tooltip=\"%s\",shape=rectangle,style=filled,fixedsize=shape]\n"
          .format(k, color, label +"\\n"+ value1, value2) +

          ins.map { dep =>
            "  %s -> %s [fontcolor=gray dir=back]".format(str(dep), k)
          }.mkString("\n") +

          outs.map { child =>
            "  %s -> %s [dir=both]".format(k, str(child))
          }.mkString("\n") +

          "\n"

        )}.mkString("\n") + "\n" +


//      sigs.map {it => (

//        "  %s [%s,label=\"%s\",tooltip=\"%s\",shape=rectangle,style=filled,fixedsize=shape]\n"
//          .format(str(it), color(it), form(it) +"\\n"+ serialize(it.value)._1, serialize(it.value)._2) +

//          it.getIns.filter { father => !father.getOuts.contains(it) }
//            .map { dep =>
//              "  %s -> %s [color=gray dir=back]".format(str(dep), str(it))
//            }.mkString("\n") +

//          it.getOuts.map { child =>
//            "  %s -> %s [dir=both]".format(str(it), str(child))
//          }.mkString("\n") +

////          (it match {
////            case it: Store[_,_] => // oops
////              it.getVars.map { child =>
////                "  %s -> %s [color=aqua]".format(str(it), str(child.underlying))
////              }.mkString("\n")
////            case _ => ""
////          })+

//          "\n"

//        )}.mkString("\n") + "\n" +

      "\n}\n\n")
  }

  //////////////////////////////////////////////////////////////////////////////
  // platform specific

  private[drx] val debugRxs = platform.platform.PotentialWeakHashMap[InternalRx[_], Unit]()

  var hook: String=>Unit = { _ => }
  def writeToDisk(str: String): Unit = {
    hook(str)
    platform.platform.writeToDisk(str)
  }

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
