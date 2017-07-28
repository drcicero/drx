//resolvers += Resolver.bintrayRepo("rmgk", "maven")
//libraryDependencies += "de.tuda.stg" %%  "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"

//enablePlugins(ScalaJSPlugin)
//name         := "all"
//scalaVersion := "2.11.8"
//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
//libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
//scalaJSUseMainModuleInitializer := true
//mainClass in (Compile, run) := Some("main.JSMain")

name         := "all"
scalaVersion := "2.11.8"
//libraryDependencies += "com.storm-enroute" %% "coroutines"  % "0.7"
mainClass in (Compile, run) := Some("main.Main")

//lazy val all = crossProject.in(file(".")).settings {
//    name         := "all"
//    scalaVersion := "2.11.8"
//  }
//  .jsSettings {
//    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
//    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
//    scalaJSUseMainModuleInitializer := true
//    mainClass in (Compile, run) := Some("main.JSMain")
//    // scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
//  }
//  .jvmSettings {
//    // libraryDependencies += "com.storm-enroute" %% "coroutines"  % "0.7"
//    mainClass in (Compile, run) := Some("main.Main")
//  }
//
//lazy val allJS = all.js
//lazy val allJVM = all.jvm
