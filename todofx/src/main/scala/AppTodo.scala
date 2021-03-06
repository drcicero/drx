import javafx.application.Application
import javafx.scene.layout.StackPane
import javafx.scene.Scene
import javafx.stage.Stage
import drx._
import javafx.geometry.Insets
import javafx.scene.control.ScrollPane
import platform.FXGUI

class AppTodo extends Application {

  def startWithA(primaryStage: Stage, title: String, box: javafx.scene.Node): Unit = {
    concreteplatform.storePrimaryStage(primaryStage)
    FXGUI.init(box)
    //concreteplatform.fpost("test", "test")

    //    val gcbuttext = Var("collect")
    //    val gcbut = rxButton(gcbuttext, { () =>
    //      concreteplatform.gc()
    //      gcbuttext set concreteplatform.heapSize().toString
    //    })

    //    val allOtherModels: Rx[Map[String, Task]] =
    //       drx.Remote.sharedAs(Todolist.model.diffs, "todos", () => Todolist.model.sampleAsDelta)
    //         //.filter(x => x._1 != drx.Network.thisClient)
    //         .scan(Map[String, Task]()){ (state, event) =>
    //           println("holaaaaa" + (state, event))
    //           IncMap.add(state, event._2) }
    //    drx.Remote.startHeartbeat()

    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new ScrollPane())
    platform.FXGUI.insertChild(root, box)
    primaryStage.setTitle(title)
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

  override def start(primaryStage: Stage): Unit = {
    startWithA(primaryStage, "abstract pull todo js", drx.interface.ATodoApp.mkATodoApp(platform.FXGUI))
  }

}