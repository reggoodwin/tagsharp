package org.tagsharp.jsoup

import scala.annotation.tailrec
import org.jsoup.nodes.TextNode

/**
 * Represents the start and end substring of the text in a TextNode.
 * To be precise this case class is composed of a Span + TextNode but
 * cannot get multiple constructors to work.
 */
sealed case class TextSpan(start: Int, end: Int, node: TextNode) {

  def overlaps(that: TextSpan):Boolean =
    (that.start < end && start < that.end) ||
      (start < that.end && that.start < end)

  def text:String = node.text().substring(start, end)

}