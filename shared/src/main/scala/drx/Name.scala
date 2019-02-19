package drx

import sourcecode.{Compat, Line, SourceCompanion, SourceValue}

import scala.language.experimental.macros

case class Name(f: sourcecode.File, l:sourcecode.Line, e:sourcecode.Name) {
  override def toString: String = {
    val ff = f.value.substring(f.value.lastIndexOf("/")+1)
    val fff = ff.substring(0, ff.indexOf("."))
    s"${e.value}\\n($fff:${l.value})"
  }
}

object Name {
  implicit def getName[T](implicit f: sourcecode.File,
                                   l: sourcecode.Line,
                                   e: sourcecode.Name): Name =
    Name(f, l, e)
}
