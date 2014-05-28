package org.tagsharp.reporter

import org.jsoup.nodes.TextNode
import org.tagsharp.test._
import org.tagsharp.test.TextError
import org.tagsharp.test.Fail
import org.tagsharp.test.ElementError
import org.tagsharp.test.Pass
import org.tagsharp.jsoup.TextSpan
import org.tagsharp.util.Span
import org.tagsharp.reporter.ErrorTagger._
import org.tagsharp.jsoup.JSoupUtil.appendAttribute
import org.tagsharp.checkpoint.Checkpoint


/**
 * Tags the errors in a Document so that error reporters know where
 * to find them, or indeed error highlighting can be outsourced to a client.
 *
 * @param wrappingTag -
 *        the tag to wrap TextNode errors in.
 * @param wrapElements -
 *        if true then element and attrMatch errors are also wrapped in the wrappingTag.
 */
class ErrorTagger(wrappingTag: String = SpanWrapper, wrapElements: Boolean = false) {

  /**
   * Modifies elements, attributes and text node identified in errors by
   * appending tag attributes to indicate the location of the errors
   * in the Document.
   */
  def tagErrors(results: List[(Checkpoint,Result)]) {

    // 1. Tag errors at the element and attrMatch level.

    for ((checkpoint,result) <- results) result match {
      case Pass() =>
      case Fail(errors) => errors foreach {
        case e: ElementError => tag(checkpoint, e)
        case _ =>
      }
    }

    // 2. Tag errors found in TextNode

    val textErrors = for {
      (c:Checkpoint, f:Fail) <- results  // also filter out Pass type
      e <- f.errors
      if (e.isInstanceOf[TextError])
    } yield (c, e.asInstanceOf[TextError])

    tag(textErrors)
  }

  /**
   * Inserts or appends a highlighting style attrMatch to the given node.
   * If wrapElements is true then the element is wrapped in the named tag
   * and the attributes are inserted or appended to the wrapping tag instead.
   * This latter option is to support a source view page highlighting.
   */
  def tag(checkpoint: Checkpoint, error: ElementError) {
    val element =
      if (!wrapElements) error.element
      else {
        error.element.wrap(wrappingTag)
        error.element.parent
      }
    appendAttribute(element, ErrorAttribute, checkpoint.id)
  }

  /**
   * Introduces error tags into a list of TextError.
   * Has to be applied to all non-overlapping and
   * overlapping matches in a TextNode simultaneously,
   * hence the re-ordering of the data structure prior to tagging.
   */
  def tag(list: List[(Checkpoint, TextError)]) = {

    // 1. Extract TextSpan from TextError
    val extracted: List[(Checkpoint, TextSpan)] = list map (p => (p._1, p._2.textSpan))

    // 2. Group TextSpan with same TextNode, so all tags applied together
    val grouped: Map[TextNode, List[(Checkpoint, TextSpan)]] = extracted.groupBy(t => t._2.node)

    // 3. Check and merge match conflicts on same TextNode
    grouped foreach { p => {
        val textNode = p._1
        val pairConverts = p._2 map (m => (m._1, Span(m._2.start, m._2.end)))
        val pairsMerged: MergedPairs = mergedPairs(pairConverts)
        // 4. Apply tags to TextNode
        applyTextNodeTags(textNode, pairsMerged)
      }
    }
  }

  type MergedPairs = List[(List[Checkpoint], Span)]

  /**
   * Performs a single pass merge of overlapping Span(s) into a single
   * all-encompassing Span that is associated all the Test associated
   * with the individual Span(s) of which this final merged Span is formed.
   *
   * It is easier to reason about and unit test Span(s) than TextSpan(es),
   * especially as all TextSpan would refer to the same TextNode anyway.
   *
   * @param list -
   *    a List of tuples of Checkpoint and Span where a Span represents
   *    a match in a TextNode and the Checkpoint accompanies it so that
   *    the context is not lost. All Span are presumed to associate
   *    with the same TextNode.
   *
   * @return -
   *    a list of merged Span associated with all the Test that are
   *    occurring on the combined merged Span, sorted in ascending
   *    Span start index order.
   */
  def mergedPairs(list: List[(Checkpoint, Span)]):MergedPairs = {

    val listSorted = list.sortWith(_._2.start < _._2.start)

    val listMerged = listSorted.foldLeft[MergedPairs](Nil) {
      (acc:MergedPairs, p:(Checkpoint, Span)) => {
        val checkpoint = p._1
        val node = p._2
        acc match {
          case Nil => List((List(checkpoint), node))
          case (tests, hnode) :: tail => {
            if (!(hnode overlaps (node))) (List(checkpoint), node) :: acc
            else {
              val tests2 = if (tests contains (checkpoint)) tests else (checkpoint :: tests)
              (tests2, node merge (hnode)) :: tail
            }
          }
        }
      }
    }

    // Return with pairs ascending
    listMerged.sortWith(_._2.start < _._2.start)
  }

  /**
   * Performs highlighting on a TextNode by splitting into N TextNode.
   * Each of the new TextNode relating to Span match are wrapped in a highlighting span.
   * Interpolated TextNode to be added in between the new TextNode.
   * Finally, the old TextNode is removed.
   *
   * Example:
   * <pre>
   *
   *   A single TextNode with errors in the words 'paragraph' and 'text':
   *
   *   <other>
   *     |_[A paragraph of text for testing.]
   *
   *   After tagging, the errors are split into separate TextNode
   *   and wrapped in a tagging span:
   *
   *   <other>
   *     |_[A ]
   *     |_<span data-errors="1234,4321">
   *          |_[paragraph]
   *     |_[ of ]
   *     |_<span data-errors="1234">
   *          |_[text]
   *     |_[ for testing.]
   *
   * </pre>
   *
   * TODO:
   * For matches on entire TextNode, should apply
   * to parent instead of wrapping TextNode in span.
   *
   * Internally this uses foldLeft as a foreach for simultaneous old/new Span access.
   *
   */
  def applyTextNodeTags(textNode: TextNode, matches: List[(List[Checkpoint], Span)]) {

    val text = textNode.getWholeText

    val lastPair = matches.foldLeft[Span](Span(0,0)) {
      (old:Span, m:(List[Checkpoint], Span)) =>
        val checkpoints = m._1
        val pair = m._2
        if (pair.start - old.end > 0) {
          textNode.before(text.substring(old.end, pair.start)) // Insert interpolated text
        }
        textNode.before(text.substring(pair.start, pair.end))
        val prev = textNode.previousSibling()
        prev.wrap(wrappingTag)
        val styleNode = prev.parent
        val ids = checkpoints.map(_.id).mkString(",")
        appendAttribute(styleNode, ErrorAttribute, ids)
        pair
    }

    if (lastPair.end < text.length) {
      textNode.before(text.substring(lastPair.end, text.length)) // Insert final interpolation
    }

    textNode.remove() // remove old textNode now that it has been split up
  }

}

object ErrorTagger {

  def tagWrapper(tag: String) = s"<$tag></$tag>"
  val SpanWrapper = tagWrapper("span")
  val ErrorTag = "error-tag"
  val ErrorTagWrapper = tagWrapper(ErrorTag)
  val ErrorAttribute = "data-error"
  val TitleAttribute = "title"

  def checkpointsFor(checkpoints: List[Checkpoint], ids: String):List[Checkpoint] = {
    if (ids.trim.length == 0) Nil
    else (for {
      id <- ids.split("""\s*,\s*""")
      c <- checkpoints.find(_.id == id)
    } yield (c)).toList
  }

}