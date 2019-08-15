import java.nio.file.{Files, Path, Paths}

import javafx.animation.{KeyFrame, Timeline}

import scala.collection.JavaConverters._
import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.Scene
import javafx.scene.layout.{StackPane, VBox}
import javafx.stage.Stage
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.scene.image.ImageView
import javafx.util.Duration

class AppMonitor extends Application {

  private def after[X](millis: Double)(func: () => X): Unit = {
    new Timeline(new KeyFrame(Duration.millis(millis), _ => func())).play()
  }

  def parseHisto(str: String): (Long, Map[String, Int]) = {
    val lines = str.strip() split "\n"
    val usedMemory = lines(0).toLong
    val instanceCount = lines drop 1 map { x: String =>
      val Array(_, amount, bytes, classname) = x split "  +"
      classname.strip() -> amount.strip().toInt }
    (usedMemory, instanceCount.toMap)
  }

  override def start(primaryStage: Stage): Unit = {
    val imageView = new ImageView()

    def onExists(file: Path)(doit: Path => Path): Unit = {
      after(1000) { () =>
        var file2 = file
        while (Files.exists(file2)) file2 = doit(file2)
        onExists(file2)(doit)
      }
    }

//    onExists(Paths.get("debuggraphs/graph1.dot.png")) { file =>
//      val image = new Image("file:///" + file.toAbsolutePath.toString)
//      imageView.setImage(image)
//      file
//    }

    //defining a series
    val varsSeries = new XYChart.Series[Number, Number]; varsSeries.setName("Live RawVars")
    val valsSeries = new XYChart.Series[Number, Number]; valsSeries.setName("Live Vals / 10")
    val memSeries = new XYChart.Series[Number, Number]; memSeries.setName("Memory in MB")

    val newest = Files.list(Paths.get("debuggraphs")).iterator().asScala.toSeq
      .filter(_.toString.startsWith("debuggraphs/histo-"))
      .maxBy(Files.getLastModifiedTime(_))
    val id = newest.toString.substring(18, newest.toString.indexOf('-', 18))
    var j = 1
    onExists(Paths.get(s"debuggraphs/histo-$id-$j.txt")) { file =>
      j += 1
      val (usedMem, map) = parseHisto(Files.readString(file))
      varsSeries.getData.add(new XYChart.Data(j, map.getOrElse("drx.pull.PRawVar", 0).asInstanceOf[Int]))
      valsSeries.getData.add(new XYChart.Data(j, map.getOrElse("drx.pull.PVal", 0).asInstanceOf[Int] / 10d))
      memSeries.getData.add(new XYChart.Data(j, usedMem / 1000d / 1000d))
      Paths.get(s"debuggraphs/histo-$id-$j.txt")
    }

    val xAxis = new NumberAxis
    val yAxis = new NumberAxis
    xAxis.setLabel("Tick [tick]")

    val lineChart = new LineChart[Number, Number](xAxis, yAxis)
    lineChart.setTitle("Memory over Time")
    lineChart.getData.addAll(varsSeries, valsSeries, memSeries)

    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new VBox(10, lineChart, imageView))
    primaryStage.setTitle("AppTodo")
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

}