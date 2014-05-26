package org.tagsharp.util


object TagUtil {

  def tagOpenRegex(tag: String):String = (s"(?si)<($tag)(\\s+[\\d\\D]*?\\s*)*>")
  def tagCloseRegex(tag: String):String = (s"(?si)<\\/($tag)\\s*>")

  def tagOpenEscapedRegex(tag: String):String = (s"(?si)&lt;($tag)(\\s+[\\d\\D]*?){0,1}&gt;")
  def tagCloseEscapedRegex(tag: String):String = (s"(?si)&lt;\\/($tag)\\s*&gt;")

  def replaceEscapedOpenTag(tag:String)(source: String):String =
    source.replaceAll(
      tagOpenEscapedRegex(tag),
      "<$1$2>"
    )

  def replaceEscapedCloseTag(tag:String)(source: String):String =
    source.replaceAll(
      tagCloseEscapedRegex(tag),
      "</$1>"
    )

}
