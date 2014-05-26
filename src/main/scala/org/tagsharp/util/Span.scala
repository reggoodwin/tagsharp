package org.tagsharp.util

import scala.math.{max, min}

/**
 * A Span represents a position in a String for denoting a substring in a TextNode.
 */
sealed case class Span(start: Int, end: Int) {

  def overlaps(other: Span):Boolean =
      (other.start < end && start < other.end) ||
      (start < other.end && other.start < end)

  def merge(other: Span):Span = {
    require(other overlaps this)
    Span(min(start, other.start), max(end, other.end))
  }

}
