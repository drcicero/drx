import RxDom.insertChild
import scalatags.JsDom.all._
import org.scalajs.dom
import platform.platform

/** Created by david on 05.05.17. */

// TODO hm, folds must be toplevel or inside Extra.lazyExtAttr blocks...?

object Main {

  def main(args: Array[String]): Unit = {
    dom.document.body.appendChild(
      button("measurements", onclick:={ () => dom.window.alert(""+platform.measurements) }).render
    )
    insertChild(dom.document.body,
      div(
        button(onclick:={ () => AppTodo.main() }, "todojs"),
        button(onclick:={ () => AppChat.main() }, "chat")
      ).render)
  }

}
