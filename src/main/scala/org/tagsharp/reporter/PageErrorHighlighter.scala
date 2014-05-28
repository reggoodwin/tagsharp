package org.tagsharp.reporter

import scala.collection.JavaConverters._
import org.jsoup.nodes.{Document,Element}
import org.tagsharp.checkpoint.Checkpoint
import org.tagsharp.jsoup.JSoupUtil.{appendAttribute, appendClass, selectElements}
import org.tagsharp.reporter.ErrorTagger.{ErrorAttribute, SpanWrapper, TitleAttribute}
import org.tagsharp.reporter.PageErrorHighlighter.appendTitleAttributeMessage
import org.tagsharp.test.Result

/**
 * An error highlighter that converts errors in failed Result into markup
 * changes in the original Jsoup HTML document that will flag them up.
 */
class PageErrorHighlighter {

  import PageErrorHighlighter._

  /**
   * For each error:
   * <ul>
   *     <li>append a style attrMatch to highlight</li>
   *     <li>append a title attrMatch with message and remove title
   *     <li>attributes from children to them overriding our message
   * </ul>
   */
  def highlight(document: Document, results: List[(Checkpoint, Result)]):Unit = {

    new ErrorTagger(SpanWrapper).tagErrors(results)

    val elements = selectElements(document,"[" + ErrorAttribute + "]")
    elements.foreach(e => {
      appendClass(e, HighlightCssClass)
    })

    val checkpoints = results.map(_._1)
    appendTitleAttributeMessage(elements, checkpoints)

    if (document.select("base").isEmpty) {
      val baseUri = document.baseUri()
      if (!baseUri.isEmpty) {
        document.head.prepend(s"<base href='$baseUri'></base>")
      }
    }
    document.head.append(ErrorStyleElement)
  }

}

object PageErrorHighlighter {

  val HighlightCssClass = "error-highlight-class"

  val ErrorStyleElement = """
    <style type="text/css">
      /* Style added by TagSharp */
      .error-highlight-class {
        padding: 1px 3px 1px 3px; 
        background-color: #FFEEEE !important;
        border: solid 1px #FF1100 !important;
      }
    </style>
    """

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