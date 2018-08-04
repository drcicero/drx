import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import org.scalajs.jsenv.nodejs.NodeJSEnv

//resolvers += Resolver.bintrayRepo("rmgk", "maven")
//libraryDependencies += "de.tuda.stg" %%  "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"
//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

scalaVersion := "2.12.6"

lazy val todojs = (project in file("todojs"))
  .dependsOn(allJS)
  .enablePlugins(ScalaJSPlugin)
  .settings( name := "todojs",
    cfg.warn, cfg.scalatags, cfg.upickle,
    scalaJSUseMainModuleInitializer := true
//  scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )

lazy val server = (project in file("server"))
  .settings( name := "server",
    cfg.warn, cfg.frk, cfg.scalatags, cfg.upickle, cfg.akka)

lazy val todofx = (project in file("todofx"))
  .dependsOn(allJVM)
  .settings( name := "todofx",
    cfg.warn, cfg.frk)

lazy val all = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings( name := "all",
    cfg.warn, cfg.scalatags, cfg.upickle, cfg.source)
  .jsSettings( mainClass in (Compile, run) := Some("Main2"),
    scalaJSUseMainModuleInitializer := true,
    jsEnv := new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--expose-gc"))),
    scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )
  .jvmSettings( mainClass in (Compile, run) := Some("main.Main")
  )

lazy val allJS = all.js
lazy val allJVM = all.jvm

val cfg = new {
  val warn      = scalacOptions ++= Seq("-feature", "-deprecation")
  val frk       = fork in run := true
  val upickle   = libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.6.6"
  val scalatags = libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"
  val source    = libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4"
  val akka = libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % "10.1.3",
    "com.typesafe.akka" %% "akka-stream" % "2.5.9",
    "com.typesafe.akka" %% "akka-actor" % "2.5.9",
  )
}

