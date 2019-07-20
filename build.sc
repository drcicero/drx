// build.sc
import mill._, scalalib._, scalajslib._

object jvm extends CrossSbtModule {
  def ivyDeps = Agg(cfg.upickle, cfg.source) ++ cfg.akka
  def crossScalaVersion = "2.12.8"
  def platformSegment = "jvm"
  def unmanagedClasspath = cfg.javafx

  def sources = T.sources(
    millSourcePath / platformSegment / "src" / "main",
    millSourcePath / "shared" / "src" / "main"
  )
}

object native extends CrossSbtModule with ScalaNative {
  def ivyDeps = Agg(cfg.upickle, cfg.source) ++ cfg.akka
  def crossScalaVersion = "2.12.8"
  def platformSegment = "jvm"

  def sources = T.sources(
    millSourcePath / platformSegment / "src" / "main",
    millSourcePath / "shared" / "src" / "main"
  )
}

object js extends CrossSbtModule with ScalaJSModule {
  def ivyDeps = Agg(cfg.upickle, cfg.source, cfg.scalajsdom)
  def crossScalaVersion = "2.12.8"
  def platformSegment = "js"
  def scalaJSVersion = "0.6.26"

  def sources = T.sources(
    millSourcePath / platformSegment / "src" / "main",
    millSourcePath / "shared" / "src" / "main"
  )
}

object server extends ScalaModule {
  def scalaVersion = "2.12.8"
  def sources = T.sources{ millSourcePath }
  def ivyDeps = Agg(cfg.upickle, cfg.scalatags) ++ cfg.akka
}

object todofx extends ScalaModule {
  def scalaVersion = "2.12.8"
  def moduleDeps = Seq(jvm)
  def unmanagedClasspath = cfg.javafx
}

object todojs extends ScalaJSModule {
  def scalaVersion = "2.12.8"
  def scalaJSVersion = "0.6.26"
  def moduleDeps = Seq(js)
  def ivyDeps = Agg(cfg.scalatags, cfg.upickle)
}

object cfg {
  val upickle    = ivy"com.lihaoyi::upickle::0.7.1"
  val scalatags  = ivy"com.lihaoyi::scalatags::0.6.7"
  val source     = ivy"com.lihaoyi::sourcecode::0.1.4" // 0.1.5
  val akka       = Seq(ivy"com.typesafe.akka::akka-http::10.1.7",
                      ivy"com.typesafe.akka::akka-stream::2.5.20",
                      ivy"com.typesafe.akka::akka-actor::2.5.20")

  val scalajsdom = ivy"org.scala-js::scalajs-dom::0.9.2"

  // use a jvm version matching your openjfx version, currently needs 11
  // sudo apt install openjfx
  // apt show openjfx
  val javafx    = Agg.from(ammonite.ops.ls(os.Path("/usr/share/openjfx/lib/"))
                           .map(PathRef(_)))
}
