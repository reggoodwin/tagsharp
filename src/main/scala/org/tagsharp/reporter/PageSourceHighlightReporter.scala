package org.tagsharp.reporter

import scala.collection.JavaConverters._
import org.jsoup.nodes.{Document, Element}
import org.tagsharp.checkpoint.Checkpoint
import org.tagsharp.test.{Result, Test}
import org.tagsharp.jsoup.JSoupUtil.{selectElements, updateStyle}
import org.tagsharp.reporter.ErrorTagger.{tagWrapper,ErrorAttribute,ErrorTag}
import org.tagsharp.reporter.PageSourceHighlightReporter.HighlightCssRule
import org.tagsharp.util.TagUtil.{replaceEscapedOpenTag, replaceEscapedCloseTag}


class PageSourceHighlightReporter {

  val template:String =
    """
      |<html>
      |<head>
      |<title>Highlighted Page</title>
      |<style type="text/css">
      |body {
      |  font-family: Arial;
      |  font-size: 1em;
      |}
      |</style>
      |</head>
      |<body>
      |ESCAPED_HTML_PAGE_HERE
      |</body>
      |</html>
    """.stripMargin

  def create(document: Document, results: List[(Checkpoint, Result)]):String = {

    val errorTag = ErrorTag
    val errorWrapper = tagWrapper(ErrorTag)

    new ErrorTagger(wrappingTag = errorWrapper, wrapElements = true).tagErrors(results)

    val elements = selectElements(document, "[" + ErrorAttribute + "]")
    elements.foreach((e:Element) => {
      updateStyle(e, HighlightCssRule)
    })

    val checkpoints = results.map(_._1)
    PageHighlightReporter.appendTitleAttributeMessage(elements, checkpoints)

    val html = document.html()
    val html2 = escapeHtml(html)
    val html3 = replaceEscapedOpenTag(errorTag)(html2)
    val html4 = replaceEscapedCloseTag(errorTag)(html3)
    // todo: merge this stage into the close stage
    val html5 = html4.replaceAll(errorTag, "span")

    template.replaceAll("ESCAPED_HTML_PAGE_HERE", html5)
  }

  /**
   * This escape function does not work quite as I would like yet.
   */
  def escapeHtml(html: String) = html
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
        //.replace(" ", "&nbsp;") // <---- screws up the error-tag
        //.replace("\r\n", "<br/>") // <---- gets rendered inside the error-tag
        //.replace("\n", "<br/>")
    .replaceAll("\\$", "&#36;")

}

object PageSourceHighlightReporter {

  val HighlightCssRule =
    "padding: 1px 3px 1px 3px; " +
      "background-color: #FFEE08 !important; " +
      "border: solid 1px #FF1100 !important;"

}