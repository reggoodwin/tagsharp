package org.tagsharp.test

import org.jsoup.nodes.Element


abstract class Test() {

  def test(element: Element): Result

  override def toString = this.getClass.getSimpleName

}
