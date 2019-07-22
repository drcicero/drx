import RxDom.insertChild
import drx.concreteplatform
import scalatags.JsDom.all._
import org.scalajs.dom

/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

object Main {

  def main(args: Array[String]): Unit = {
    dom.document.body.appendChild(
      button("measurements", onclick:={ () =>
        dom.document.body.textContent = ""+concreteplatform.measurements.mkString(", ") }).render
    )
    dom.document.body.appendChild(
      div(
        button(onclick:={ () => AppTodo.main() }, "push todojs"),
        button(onclick:={ () => AppChat.main(true) }, "push chat sync"),
        button(onclick:={ () => AppChat.main(false) }, "push chat async"),
        button(onclick:={ () => standard.SAppTodo.main() }, "pull todo js")
      ).render)
  }

}
