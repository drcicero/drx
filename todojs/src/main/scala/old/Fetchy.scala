package old

import org.scalajs.dom.experimental.{Fetch, HttpMethod, RequestInit}

import drx.concreteplatform._
import scala.concurrent.Future
import scala.scalajs.js.typedarray.Uint8Array

object Fetchy {
  def fetchText(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.text.toFuture)

  // https://stackoverflow.com/questions/9267899/arraybuffer-to-base64-encoded-string/11562550#11562550
  def fetchBase64(url: String): Future[String] =
    Fetch.fetch(url, RequestInit(HttpMethod.GET))
      .toFuture.flatMap(x => x.arrayBuffer.toFuture).map { x =>
      val btoa = org.scalajs.dom.window.btoa _
      val fromCharCode = scalajs.js.Dynamic.global.String.fromCharCode
      btoa(fromCharCode.applyDynamic("apply")(
        (), new Uint8Array(x)).asInstanceOf[String])
    }
}
