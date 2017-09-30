package main

import drx._

/** Created by david on 10.06.17. */
object Main {
  private sealed abstract class Tree(var obsid: Sink[_] = null)
  private case class Leaf(content: String) extends Tree
  private case class Ast(children: MList) extends Tree
  private type MList = scala.collection.mutable.ListBuffer[Tree]
  private def span(x: String) = Leaf(x)
  private def div(x: Tree*) = {val arr = new MList(); arr++=x; Ast(arr)}
  private def printTree(t: Tree, i: Int = 0): Unit = t match {
    case Leaf(s) => println("|" + "  " * i + "| " + s)
    case Ast(l) => l.foreach((x) => printTree(x, i+1))
  }

  private def trender(sig: Rx[_ <: Tree]): Tree = {
    def forallObs(t: Tree, func: Sink[_] => Unit): Unit = {
      if (t.obsid != null) func(t.obsid)
      t match {
        case Ast(c) => c.foreach(forallObs(_, func))
        case Leaf(s) =>
      }
    }
    val medium = div(span("{{init}}"))
    medium.obsid = sig.mkObs({ newelem: Tree =>
      val fc = medium.children.head
      forallObs(fc, _.stop())
      forallObs(newelem, _.start())
      medium.children(0) = newelem
    })
    medium
  }

  private def tview(it: Task): Tree =
    trender(Signal(div(
      span(if (it.done.get) "-" else " "),
      span(it.title.get),
      span(if (it.done.get) "-" else " ")
    )))

  def main(): Unit = {
    Main.main(Array())
  }

  def partOfMain(): Unit = {
    object state extends VarOwner {
      val model: Variable[List[Task]] = mkVar(List(), "model")
    }

    val mapped = state.model
      .map({ it => it.length }, "length")
      .map({ it => if (it == 0) "No" else ""+it }, "string")
      .map({ it => span(it) }, "span")

    val screen = div(
      span("DO TODOs!"),
      div(span("There are "), trender(mapped), span(" todos left")),

      trender(Signal(
        if (state.model.get.isEmpty)
          span("All done! :)")
        else
          trender(Signal(div(state.model.get.map { it => tview(it) }:_*)))
      ))
    )

    drx.debug.doit()

    for (x <- 1 to 3) {
      for (x <- 1 to 20) state.model.transform(x => x ++ List(new Task("hello")))
      drx.debug.doit()

      for (x <- 1 to 20) state.model.transform(_.tail)
      drx.debug.doit()
    }

    drx.debug.doit()
  }

  def main(args: Array[String]): Unit = {
    val daa = new Variable[Boolean](false)
    val a = daa.map(!_).map(!_).map(!_).map(!_)
    val b = a.map(!_)
    val c = a.map(!_)
    val d = Extras.zip(c,b).map(_._1)
    val e = d.map(!_)
    val f = e.map(!_)
    transact {
      daa set true
      println("interest: " + f.sample + " == true")
    }

    val t1 = new Channel[Boolean]()
    val t2 = t1 // .fold(false)((state,ev)=>ev)

    val a1 = t2
      .changes()
      .map(it=>it)
      .map(it=>it)
      .hold(false)
      .map(it=>it)
      .map(it=>it)
      .changes()
      .map(it=>it)
      .map(it=>it)

    println("interest: " + a1.sample + " == false")
    t1 send true
    println("interest: " + a1.sample + " == true")
    t1 send false
    println("interest: " + a1.sample + " == false")

//    val t3 = new Source[Int]()
//    val t4 = t3.map(it=>it)
//    val t5 = t4.map(it=>it)
//
//    val t6 = new Source[Int]()
//    val t7 = Signal(if (t1.get) t5.get else t6.get)
//    t7.observe { it => println("!!! " + it) }
//
//    t3.fire(2)
//    t6.fire(3)
//    t3.fire(2)
//    t6.fire(3)
//    t1.fire(true)
//    t3.fire(2)
//    t6.fire(3)
//    t3.fire(2)
//    t6.fire(3)
//    t1.fire(false)
//    t3.fire(2)
//    t6.fire(3)
//    t3.fire(2)
//    t6.fire(3)
//    t1.fire(false)
//    t3.fire(2)
//    t6.fire(3)
//    t3.fire(2)
//    t6.fire(3)
//    t1.fire(true)
//    t3.fire(2)
//    t6.fire(3)
//    t3.fire(2)
//    t6.fire(3)

//    for (x <- 1 to 2) {
//      partOfMain()
//    }

//    var keeprunning = true
//    while (keeprunning) {
//      printTree(screen)
//      println()

//      drx.printless()
//      println()

//      var input = scala.io.StdIn.readLine("q/n/0-9? ")
//      if (input == "q")
//        keeprunning = false
//      else if (input.startsWith("n"))
//        model.transform( x => x ++ List(new Task(input)))
//      else {
//        var x = input.split(" ", 2)(0).toInt
//        model.now(x).title.set( input.split(" ", 2)(1).trim() )
//        model.transform( list => list.filter( e => e.title.now != "" ) )
//      }
//    }
  }

}
