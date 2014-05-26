package org.tagsharp.test

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.Jsoup
import org.tagsharp.jsoup.PackedText

/**
 * Wraps a Jsoup document, a convenience access to concatenated body
 * text and later on a testable page section.
 */
class HtmlPage(val html: String) {

  lazy val document: Document = Jsoup.parse(html)

  /**
   * The default section is the whole HTML &lt;body&gt;
   */
  lazy val section: Element = document.body()

  /**
   * A concatenated view of the &lt;body&gt; text for regex
   */
  lazy val bodyText = new PackedText(document.body())

}
