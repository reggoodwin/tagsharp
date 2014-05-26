package org.tagsharp.reporter

import scala.collection.JavaConverters._
import org.jsoup.nodes.{Document,Element}
import org.tagsharp.checkpoint.Checkpoint
import org.tagsharp.jsoup.JSoupUtil.{appendAttribute, selectElements, updateStyle}
import org.tagsharp.reporter.ErrorTagger.{ErrorAttribute, SpanWrapper, TitleAttribute}
import org.tagsharp.reporter.PageHighlightReporter.appendTitleAttributeMessage
import org.tagsharp.reporter.PageHighlightReporter.HighlightCssRule
import org.tagsharp.test.Result

/**
 * An error Reporter that reports on errors by highlighting
 * them in the original HTML document.
 */
class PageHighlightReporter {

  /**
   * For each error:
   * <ul>
   *     <li>append a style attrMatch to highlight</li>
   *     <li>append a title attrMatch with message and remove title
   *     <li>attributes from children to them overriding our message
   * </ul>
   */
  def create(document: Document, results: List[(Checkpoint, Result)]) {

    new ErrorTagger(SpanWrapper).tagErrors(results)

    val elements = selectElements(document,"[" + ErrorAttribute + "]")
    elements.foreach(e => {
      updateStyle(e, HighlightCssRule)
    })

    val checkpoints = results.map(_._1)
    appendTitleAttributeMessage(elements, checkpoints)

    if (document.select("base").isEmpty) {
      val baseUri = document.baseUri()
      if (!baseUri.isEmpty) {
        document.head.prepend(s"<base href='$baseUri'></base>")
      }
    }

  }

}

object PageHighlightReporter {

  val HighlightCssRule =
    "padding: 1px 3px 1px 3px; " +
    "background-color: #FFEE08 !important; " +
    "border: solid 1px #FF1100 !important;"

  // For page and source view
  def appendTitleAttributeMessage(elements: List[Element], checkpoints: List[Checkpoint]) = {
    elements.foreach(e => {
      val ids = e.attr(ErrorAttribute)
      val children = e.children().asScala.toList
      val msg = ErrorTagger.checkpointsFor(checkpoints, ids).map(_.name).mkString("\n")
      appendAttribute(e, TitleAttribute, msg, "")
      children.foreach(_.removeAttr(TitleAttribute)) // to prevent override
    })
  }

}