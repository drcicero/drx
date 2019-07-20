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
    insertChild(dom.document.body,
      div(
        button(onclick:={ () => AppTodo.main() }, "todojs"),
        button(onclick:={ () => AppChat.main(true) }, "chat sync"),
        button(onclick:={ () => AppChat.main(false) }, "chat sync")
      ).render)
  }

}
