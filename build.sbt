import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import org.scalajs.jsenv.nodejs.NodeJSEnv

target := file("/tmp/sbt") / name.value
scalaVersion := "2.12.8"

lazy val todojs = (project in file("todojs"))
  .dependsOn(allJS)
  .enablePlugins(ScalaJSPlugin)
  .settings( name := "todojs", cfg.common,
    cfg.scalatags, cfg.upickle,
    scalaJSUseMainModuleInitializer := true,
    scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )

lazy val server = (project in file("server"))
  .settings( name := "server", cfg.common,
    cfg.scalatags, cfg.upickle, cfg.akka,
    connectInput in run := true)

lazy val todofx = (project in file("todofx"))
  .settings(cfg.check)
  .dependsOn(allJVM)
  .settings( name := "todofx", cfg.common)

lazy val all = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings( name := "all",
    cfg.scalatags, cfg.upickle, cfg.source)
  .jsSettings( mainClass in (Compile, run) := Some("Main2"), cfg.common,
    scalaJSUseMainModuleInitializer := true,
    jsEnv := new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--expose-gc"))),
    scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )
  .jvmSettings( mainClass in (Compile, run) := Some("main.Main"), cfg.common,
    cfg.javafx, cfg.akka,
  )

lazy val allJS = all.js
lazy val allJVM = all.jvm

cancelable in Global := true

val cfg = new {
  val common    = scalacOptions ++= Seq("-feature", "-deprecation")
//val reflect   = libraryDependencies += "org.scala-lang" %%% "scala-reflect" % "2.12.6"
  val upickle   = libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.7.1"
  val scalatags = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"
  val source    = libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4" // 0.1.5
  val check     = libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"
  val javafx    = unmanagedJars in Compile ++= Attributed.blankSeq(sbt.IO.listFiles(file("/usr/share/openjfx/lib/")))
  val akka = libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % "10.1.7",
    "com.typesafe.akka" %% "akka-stream" % "2.5.20",
    "com.typesafe.akka" %% "akka-actor" % "2.5.20",
  )
}
