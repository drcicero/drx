//resolvers += Resolver.bintrayRepo("rmgk", "maven")
//libraryDependencies += "de.tuda.stg" %% "rescala" % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescala" % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"

enablePlugins(ScalaJSPlugin)
libraryDependencies += "org.scala-js"      %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "com.lihaoyi"       %%% "scalatags"   % "0.6.5"

name := "all"
version := "0.2"
scalaVersion:= "2.11.8"

lazy val all = project.in(file(".")).aggregate(drxJS, drxJVM)

lazy val drx = crossProject.in(file("."))
  .jsSettings {
    mainClass in (Compile, run) := Some("main.JSMain")
    scalaJSUseMainModuleInitializer := true
//    libraryDependencies += "com.storm-enroute" %% "coroutines" % "0.6"
//    scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  }
  .jvmSettings {
    libraryDependencies += "com.storm-enroute" %%% "coroutines"  % "0.6"
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
    mainClass in (Compile,run) := Some("main.Main")
  }
lazy val drxJS = drx.js
lazy val drxJVM = drx.jvm
