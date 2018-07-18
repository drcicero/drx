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
  .settings( name := "todojs"
    , scalacOptions ++= Seq("-feature", "-deprecation")
    , scalaJSUseMainModuleInitializer := true
    , libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
//  , scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )

lazy val todofx = (project in file("todofx"))
  .dependsOn(allJVM)
  .settings( name := "todofx"
    , scalacOptions ++= Seq("-feature", "-deprecation")
    , fork in run := true
  )

lazy val all = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings( name := "all"
    , scalacOptions ++= Seq("-feature", "-deprecation")
    , libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.6.6"
  )
  .jsSettings( mainClass in (Compile, run) := Some("Main2")
    , scalaJSUseMainModuleInitializer := true
    , libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"
    , jsEnv := new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--expose-gc")))
    , scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )
  .jvmSettings( mainClass in (Compile, run) := Some("main.Main")
  )

lazy val allJS = all.js
lazy val allJVM = all.jvm

