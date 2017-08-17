package main

import drx.{Signal, Var, Observer, Token}

/** Created by david on 10.06.17. */
object Main {
  private sealed abstract class Tree(var obsid: Observer[_] = null)
  private case class Leaf(content: String) extends Tree
  private case class Ast(children: MList) extends Tree
  private type MList = scala.collection.mutable.ListBuffer[Tree]
  private def span(x: String) = Leaf(x)
  private def div(x: Tree*) = {val arr = new MList(); arr++=x; Ast(arr)}
  private def printTree(t: Tree, i: Int = 0): Unit = t match {
    case Leaf(s) => println("|" + "  " * i + "| " + s)
    case Ast(l) => l.foreach((x) => printTree(x, i+1))
  }

  private val mytoken = new Token("rendering")
  private def trender(sig: Signal[_ <: Tree]): Tree = {
    def forallObs(t: Tree, func: Observer[_] => Unit): Unit = {
      if (t.obsid != null) func(t.obsid)
      t match {
        case Ast(c) => c.foreach(forallObs(_, func))
        case Leaf(s) =>
      }
    }
    val medium = div(span("{{init}}"))
    medium.obsid = sig.observe({ newelem: Tree =>
      val fc = medium.children.head
      forallObs(fc, _.deactivate(mytoken))
      forallObs(newelem, _.activate(mytoken))
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
    val model = new Var(List[Task](), "model")
    val mapped = model
      .map({ it => it.length }, "length")
      .map({ it => if (it == 0) "No" else ""+it }, "string")
      .map({ it => span(it) }, "span")

    val screen = div(
      span("DO TODOs!"),
      div(span("There are "), trender(mapped), span(" todos left")),

      trender(Signal(
        if (model.get.isEmpty)
          span("All done! :)")
        else
          trender(Signal(div(model.get.map { it => tview(it) }:_*)))
      ))
    )

    drx.debug.doit()

    for (x <- 1 to 3) {
      for (x <- 1 to 20) model.transform(x => x ++ List(new Task("hello")))
      drx.debug.doit()

      for (x <- 1 to 20) model.transform(_.tail)
      drx.debug.doit()
    }

    drx.debug.doit()
  }

  def main(args: Array[String]): Unit = {
//    for (x <- 1 to 2) {
      partOfMain()
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
