//resolvers += Resolver.bintrayRepo("rmgk", "maven")
//libraryDependencies += "de.tuda.stg" %%  "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"

//name         := "all"
//scalaVersion := "2.11.8"
//mainClass in (Compile, run) := Some("main.Main")

//enablePlugins(ScalaJSPlugin)
//scalaJSUseMainModuleInitializer := true
//jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv("node", List("--expose-gc"))

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

lazy val todo = (project in file("todoapp"))
  .dependsOn(allJS)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name         := "todoapp",
    scalaVersion := "2.12.2",
    scalaJSUseMainModuleInitializer := true,
    // mainClass in (Compile, run) := Some("TodoApp"),
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
    // scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )

lazy val all = crossProject.in(file(".")).settings(
    name         := "all",
    scalaVersion := "2.12.2"
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    mainClass in (Compile, run) := Some("Main2"),
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5",
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv("node", List("--expose-gc"))
    // scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )
  .jvmSettings(
    mainClass in (Compile, run) := Some("main.Main")
  )

lazy val allJS = all.js
lazy val allJVM = all.jvm
