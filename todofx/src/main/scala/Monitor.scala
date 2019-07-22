import java.nio.file.{Files, Paths}

import javafx.animation.KeyFrame
import javafx.animation.Timeline
import javafx.util.Duration
import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.Insets
import javafx.scene.Scene
import javafx.scene.layout.{HBox, StackPane, VBox}
import javafx.stage.Stage
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.scene.image.Image
import javafx.scene.image.ImageView

object MonitorMain {
  def main(args: Array[String]): Unit =
  Application.launch(classOf[Monitor], args:_*)
}

class Monitor extends Application {

  override def start(primaryStage: Stage): Unit = {
    val image = new Image("file:///" + Paths.get("debuggraphs/graph1.dot.png").toAbsolutePath.toString)
    val iv1 = new ImageView()
    iv1.setImage(image)

    def dolater(ms: Int)(f: EventHandler[ActionEvent]): Unit = {
      val timeline = new Timeline(new KeyFrame(Duration.millis(ms), f))
      timeline.play()
    }

    def f(): Unit = {
      dolater(1000) { _ =>
        val mod = Files.getLastModifiedTime(Paths.get("debuggraphs/graph1.dot.png"))
        println(mod)
        f()
      }
    }
    f()

    //defining a series
    val series = new XYChart.Series[Number, Number]
    series.setName("Memory [MB]")
    series.getData.add(new XYChart.Data(1, 23))
    series.getData.add(new XYChart.Data(2, 14))
    series.getData.add(new XYChart.Data(3, 15))
    series.getData.add(new XYChart.Data(4, 24))
    series.getData.add(new XYChart.Data(5, 34))
    series.getData.add(new XYChart.Data(6, 36))
    series.getData.add(new XYChart.Data(7, 22))
    series.getData.add(new XYChart.Data(8, 45))
    series.getData.add(new XYChart.Data(9, 43))
    series.getData.add(new XYChart.Data(10, 17))
    series.getData.add(new XYChart.Data(11, 29))
    series.getData.add(new XYChart.Data(12, 25))

    val xAxis = new NumberAxis
    val yAxis = new NumberAxis
    xAxis.setLabel("Tick [tick]")

    val lineChart = new LineChart[Number, Number](xAxis, yAxis)
    lineChart.setTitle("Memory over Time")
    lineChart.getData.add(series)

    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new VBox(10, lineChart, iv1))
    primaryStage.setTitle("AppTodo")
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

}