package org.tagsharp.test

import org.jsoup.nodes.Element


abstract class Test() {

  def apply(element: Element): Result

  override def toString = this.getClass.getSimpleName

}
