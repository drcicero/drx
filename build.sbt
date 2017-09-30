//resolvers += Resolver.bintrayRepo("rmgk", "maven")
//libraryDependencies += "de.tuda.stg" %%  "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescala"     % "0.20.0-SNAPSHOT"
//libraryDependencies += "de.tuda.stg" %%% "rescalatags" % "0.20.0-SNAPSHOT"
//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

lazy val todojs = (project in file("todojs"))
  .dependsOn(allJS)
  .enablePlugins(ScalaJSPlugin)
  .settings( name := "todojs"
    , scalaVersion := "2.12.2"
    , scalaJSUseMainModuleInitializer := true
    , libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
    , scalacOptions += "-feature"
//  , scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )

lazy val todofx = (project in file("todofx"))
  .dependsOn(allJVM)
  .settings( name := "todofx"
    , scalaVersion := "2.12.2"
  )

lazy val all = crossProject.in(file(".")).settings( name := "all"
    , scalacOptions += "-feature"
    , scalaVersion := "2.12.2"
  )
  .jsSettings( mainClass in (Compile, run) := Some("Main2")
    , scalaJSUseMainModuleInitializer := true
    , libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"
    , jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv("node", List("--expose-gc"))
    , scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) }
  )
  .jvmSettings( mainClass in (Compile, run) := Some("main.Main")
    , unmanagedJars in Compile += Attributed.blank(file("/home/david/.IdeaIC2016.1/system/restart/jre/lib/ext/jfxrt.jar"))
  )

lazy val allJS = all.js
lazy val allJVM = all.jvm
