package drx

import drx.graph.{RxDynamic, Obs}
import drx.internals.EmptyValExc

import scala.collection.mutable
import scala.util.{Failure, Success}

/** Created by david on 10.06.17. */
object debug {
  val evaluating: mutable.Set[RxDynamic[_]] = mutable.Set()

  def ellipsis(tmp: String): String =
    if (tmp.length < 10) tmp
    else tmp.slice(0, 7) + "..."

  def serialize(x: Any): String = x match {
    case Failure(e) if e.isInstanceOf[EmptyValExc] => "/"
    case Success(y) => serialize(y)
    case x: RxDynamic[_] => x.toString
    // case x: Node => x.outerHTML.replace('"', "'")
    case x: mutable.Map[_, _] =>
      "{" + x.map {
        case (a, b) => "" + serialize(a) + ":" + serialize(b)
      }.mkString(", ") + "}"
    case x: Traversable[_] =>
      "[" + x.map(it => serialize(it)).mkString(", ") + "]"
    case _ => ("" + x).replaceAllLiterally("\"", "\\\"")
  }

  def getthem(rx: RxDynamic[_], acc: mutable.Set[RxDynamic[_]]): Unit = if (!acc.contains(rx)) {
    acc += rx
    rx.ins.foreach { y => getthem(y, acc) }
    rx.outs.foreach { y => getthem(y, acc) }
  }

  def transitivehull(xs: Set[RxDynamic[_]]): Set[RxDynamic[_]] = {
    val set = mutable.Set[RxDynamic[_]]()
    xs.foreach(getthem(_, set))
    set.toSet
  }

  //  def transitivehullobservers(xs: Set[Observer[_]]): Set[Observer[_]] = {
  //    val set = mutable.Set[DataflowNode[_]]()
  //    xs.flatMap(_.getOuts).foreach(getthem(_, set))
  //    set.flatMap(_.getOuts).collect { case (x:Observer[_]) => x }.toSet
  //  }

  def stringit(root: Set[Obs[_]] = Set(), desc: String = ""): String = {

    def color(rx: RxDynamic[_]): String =
      "color=" + (
        if (rx.isObserved) "black" // "\"#aadd00\""
        else "white"
        ) +
        ",fillcolor=" + (
        if (withInstant(_.shouldPush(rx))) "\"orange\""
        else if (rx.debugEvaluating) "\"#ddaa00\"" // orange
        //        else if (withTransaction.activeCtx.value.exists(_.ready.contains(rx))) "\"#dddd00\"" // yellow
        else if (withInstant.activeInstant.value.exists(x =>
          x.dirtySources.contains(rx) || x.dirty.contains(rx))) "\"#aadd00\"" // green
        else if (withInstant.activeInstant.value.exists(_.clean.contains(rx))) "\"gray\""
        else "white")
    //      + "," + (rx match {
    //        case _: Var[_]  => "shape=triangle"
    //        case _: Sink[_] => "shape=invtriangle"
    //        case _          => "shape=diamond"
    //      })

    def form(rx: RxDynamic[_]): String = if (rx.toString.contains("_")) rx.toString.split("_", 2)(1) else rx.toString // rx.hashCode().toString
    def str(rx: RxDynamic[_]): String = '"' + rx.toString + "_" + rx.level + '"'
    //    def str(rx: InternalRx[_]): String = rx.hashCode().toString

    def mapIt(kv: (String, collection.immutable.Set[RxDynamic[_]])) = {
      val (k, l) = kv

      def intime(f: atomic => mutable.Set[RxDynamic[_]])(it: RxDynamic[_]) =
        withInstant.activeInstant.value.exists(f(_) contains it)

      def count =
        if (l.size <= 1) ""
        else {
          val pushing = l.count(_.isObserved)
          val size = l.size
          val paused = if (size == pushing) "" else "(+" + (size - pushing) + ")"
          "" + pushing + paused + "x "
        }

      (k, (
        count +
          form(l.head) + "\\n" +
          ellipsis(l map (it => serialize(it.getIt)) mkString ", ") +
          " (" + l.map(_.debugCtr).sum + ")",
        l map (it => serialize(it.getIt)) mkString "\n",
        l flatMap (it => it.ins filter (father => !(father.outs contains it))),
        l flatMap (_.outs),
        "fillcolor=" + (
          if (l exists evaluating.contains) "\"#ddaa00\""
          else if (l exists intime(_.dirtySources)) "\"#aadd00\""
          else if (l exists intime(_.shouldPush)) "ivory3"
          else if (l exists intime(_.clean)) "ivory2"
          else "white") +

        ",color=" + (
          if (l exists (_.getIt.isSuccess)) "black"
          else if (l exists (_.getIt.failed.get.isInstanceOf[EmptyValExc])) "none"
          else "red") +
          (if (l forall (!_.isObserved)) ",fontcolor=gray" else "")
      ))
    }

    val roots = root.collect { case x: RxDynamic[_] => x }
    val sigs = transitivehull(roots ++ debugRxs.keys)
    val gsigs = sigs.groupBy(str).map(mapIt)

    // "\"" + rx.toString +"\\n"+ serialize(rx.value) + "\"" // "\"%s\\n%d\"".format(rx.toString, rx.level)

    val ranks = sigs.groupBy(x => x.level)
    val rankstr = (
      ranks.map(x => "{ rank=same %s %s }\n".format(
        x._1, x._2.map(str).mkString(" ")
      )).mkString("") +

      ranks.map(x => s"${x._1} [shape=none]\n").mkString("") +
        ranks.keys.toList.sorted.mkString(" -> ") + " [arrowhead=none]\n"
      )


    ("digraph {\n\n" +

      "graph [ fontsize=8 ]\n" +
      "edge [ fontsize=8 ]\n" +
      "node [ fontsize=8 ]\n" +
      "labelloc=\"t\";\n" +
      "label=\"" + desc.replace("\"", "'") + "\"; \n\n" +
      (if (ranks.nonEmpty) rankstr else "")+

      gsigs.map { case (k, (label, value2, ins, outs, color)) => (

        "  %s [%s,label=\"%s\",tooltip=\"%s\",shape=rectangle,style=filled,fixedsize=shape]\n"
          .format(k, color, label, value2) +

          ins.map { dep =>
            "  %s -> %s [color=gray dir=none]".format(str(dep), k)
          }.mkString("\n") +

          outs.map { child =>
            "  %s -> %s []".format(k, str(child))
          }.mkString("\n") +

          "\n"

        )
      }.mkString("\n") + "\n" +


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

  private[drx] val debugRxs = concreteplatform.WeakSetOrNot[RxDynamic[_], Unit]()

  var hook: String => Unit = { _ => }

  def writeToDisk(str: String): Unit = {
    concreteplatform.gc()
    val heap = " (" + drx.debug.debugRxs.keys.size + " nodes)"
    hook(str + heap)
    concreteplatform.writeToDisk(str + heap)
  }

  def printless(): Unit = {
    println(concreteplatform.heapSize())
    //    println("v=" + debugVars.size + " s=" + debugSigs.size +
    //      " o=" + debugObs.size + " heap=" + compat2.heapSize() / 10000000)
  }

  def doit(): Unit = {
    printless()
    for (i <- 0 to 5) concreteplatform.gc()
    printless()
  }
}
