package org.tagsharp.dsl

import org.jsoup.nodes.Element

/**
 * This augments Jsoup, whose own Attribute does not retain
 * a reference back to its parent Element. The back reference is
 * needed for error highlighting.
 */
case class Attribute(name:String, element:Element) {

  lazy val value:String = {
    element.attributes().get(name) match {
      case null => ""
      case value => value.trim
    }
  }

  def hasAttribute:Boolean = element.attributes().get(name) != null
  def hasValue:Boolean = !value.isEmpty

}
