import drx.{Signal, Var}
import main.Render.Task

/** Created by david on 10.06.17. */
object Main extends App {
  private abstract class Tree { var obsid: String = "" }
  private case class Leaf(content: String) extends Tree
  private case class Ast(children: MList) extends Tree
  private type MList = scala.collection.mutable.ListBuffer[Tree]
  private def span(x: String) = Leaf(x)
  private def div(x: Tree*) = {val arr = new MList(); arr++=x; Ast(arr)}
  private def printTree(t: Tree, i: Int = 0): Unit = t match {
    case Leaf(s) => println("|" + "  " * i + "| " + s)
    case Ast(l) => l.foreach((x) => printTree(x, i+1))
  }

  private def trender(sig: Signal[_ <: Tree]): Tree = {
    def disconnectObservers(t: Tree): Unit = {
      if (t.obsid != "") drx.debugObs.get(t.obsid) match {
        case None    => throw new RuntimeException("double free '" + t.obsid + "'")
        case Some(x) => x.disconnect()
      }
      t match {
        case Ast(c) => c.foreach(disconnectObservers)
        case Leaf(s) => ()
      }
    }
    val medium = div(span("{{init}}"))
    val obs = sig.onChange({ newelem: Tree =>
      val fc = medium.children(0)
      disconnectObservers(fc)
      medium.children(0) = newelem
    }, onDisconnect = { () =>
      medium.obsid = ""
    })
    medium.obsid = obs.id
    medium
  }

  private def tview(it: Task): Tree =
    trender(new Signal(() => div(
      span(if (it.done.get) "-" else " "),
      span(it.title.get),
      span(if (it.done.get) "-" else " ")
    )))

  override def main(args: Array[String]): Unit = {
    val model = new Var(List[Task](), "model")
    val mapped = model
      .map({ it => it.length }, "length")
      .map({ it => if (it == 0) "No" else ""+it }, "string")
      .map({ it => span(it) }, "span")

    val screen = div(
      span("DO TODOs!"),
      div(span("There are "), trender(mapped), span(" todos left")),

      trender(new Signal(() =>
        if (model.get.isEmpty)
          span("All done! :)")
        else
          trender(new Signal(() => div(model.get.map { it => tview(it) }:_*)))
      ))
    )

    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.transform(x => x ++ List(new Task("hello")))
    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.transform(x => x ++ List(new Task("baka")))
    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.transform(x => x ++ List(new Task("heyhey")))
    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.now(0).title.set( "" )
    model.transform( list => list.filter( e => e.title.now != "" ) )
    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.now(1).title.set( "" )
    model.transform( list => list.filter( e => e.title.now != "" ) )
    drx.helper.printless(); System.gc(); drx.helper.printless()

    model.now(0).title.set( "" )
    model.transform( list => list.filter( e => e.title.now != "" ) )
    drx.helper.printless(); System.gc(); drx.helper.printless()

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
