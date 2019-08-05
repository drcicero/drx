import java.nio.charset.StandardCharsets
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

class Monitor extends Application {

  private def after[X](millis: Double)(func: () => X): Unit = {
    new Timeline(new KeyFrame(Duration.millis(millis), _ => func())).play()
  }

  def parseHisto(str: String): Map[String, Int] =
    (str.strip() split "\n" map { x: String =>
      println((x split "  +").toSeq)
      val Array(_, amount, bytes, classname) = x split "  +"
      classname.strip() -> amount.strip().toInt
    }).toMap

  override def start(primaryStage: Stage): Unit = {
    val imageView = new ImageView()

    def onexists(file: Path)(doit: Path => Path): Unit = {
      after(1000) { () =>
        onexists(if (Files.exists(file)) doit(file) else file)(doit)
      }
    }

//    onexists(Paths.get("debuggraphs/graph1.dot.png")) { file =>
//      val image = new Image("file:///" + file.toAbsolutePath.toString)
//      imageView.setImage(image)
//      file
//    }

    //defining a series
    val varsSeries = new XYChart.Series[Number, Number]
    varsSeries.setName("RawVars")
    val valsSeries = new XYChart.Series[Number, Number]
    valsSeries.setName("Vals")

    val newest = Files.list(Paths.get("debuggraphs")).iterator().asScala.toSeq
      .filter(_.toString.startsWith("debuggraphs/histo-"))
      .maxBy(Files.getLastModifiedTime(_))
    val id = newest.toString.substring(18, newest.toString.indexOf('-', 18))
    var j = 1
    onexists(Paths.get(s"debuggraphs/histo-$id-$j.txt")) { file =>
      j += 1
      val map = parseHisto(Files.readString(file))
      varsSeries.getData.add(new XYChart.Data(j, map.getOrElse("drx.pull.PRawVar", 0).asInstanceOf[Int]))
      valsSeries.getData.add(new XYChart.Data(j, map.getOrElse("drx.pull.PVal", 0).asInstanceOf[Int]))
      Paths.get(s"debuggraphs/histo-$id-$j.txt")
    }

    val xAxis = new NumberAxis
    val yAxis = new NumberAxis
    xAxis.setLabel("Tick [tick]")

    val lineChart = new LineChart[Number, Number](xAxis, yAxis)
    lineChart.setTitle("Memory over Time")
    lineChart.getData.add(varsSeries)
    lineChart.getData.add(valsSeries)

    val root = new StackPane()
    root.setPadding(new Insets(10))
    root.getChildren.add(new VBox(10, lineChart, imageView))
    primaryStage.setTitle("AppTodo")
    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }

}