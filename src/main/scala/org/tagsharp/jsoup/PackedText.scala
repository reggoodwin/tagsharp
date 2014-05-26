package org.tagsharp.jsoup

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import org.jsoup.nodes.{Node, TextNode, Element}
import org.jsoup.select.{NodeVisitor, NodeTraversor}
import org.tagsharp.jsoup.PackedText.NodePos

/**
 * This produces a lossless concatenated view of all TextNodes in the given root Element.
 * Most likely the passed in Element will be a &lt;body&gt; element.
 * This is used as an optimising step making it faster to Regex match across a single 
 * text sequence rather than run the same regex match repeatedly across each TextNode.
 *
 * Having said that, performance gains only take effect when there are many
 * terms being sought, for example a test involving a search for a list of
 * prohibited of case sensitive terms.
 *
 * This is lossless so that a text match can be backtracked to the owning 
 * Element for error reporting later on.
 */
class PackedText(val rootElement: Element) {

  def fetch():(Array[NodePos], String) = {
    val sb = new StringBuilder
    val buffer = new ArrayBuffer[NodePos]
    val visitor = new TextNodeVisitor(sb, buffer)
    new NodeTraversor(visitor).traverse(rootElement)
    val array:Array[NodePos] = buffer.toArray // Performance: cost of change to array?
    (array, sb.toString)
  }

  private lazy val (_buffer, _allText) = fetch()
  lazy val nodePosBuffer: Array[NodePos] = _buffer // should be immutable?
  lazy val allText = _allText

  /**
   * This is a simple regex term match on each TextNode.
   * It is limited in that a match can only apply to one TextNode at a time.
   * So terms that span multiple TextNode will not be found.
   * However this is likely to be the majority use case and
   * also has the advantage that block level checks are not needed
   * to rule out false positives occurring from matches spanning multiple blocks.
   *
   * @return a List of TextSpan referencing each match in the text.
   */
  def matchNode(term: Regex): List[TextSpan] = {

    val nodes = for {
      rm:Regex.Match <- term findAllMatchIn allText
      np <- nodePosBuffer
      if (rm.start >= np.start && rm.end <= np.end)
      start = rm.start - np.start
      end = start + rm.group(0).length
    } yield TextSpan(start, end, np.node)

    nodes.toList
  }

  /**
   * Retrieves text from all the traversed Node into a StringBuilder
   * and maps their positions with an array of NodePos.
   */
  private class TextNodeVisitor(sb:StringBuilder, array: ArrayBuffer[NodePos]) extends NodeVisitor {

    override def head(node: Node, depth: Int) {}

    override def tail(node: Node, depth: Int) {
      if (node.isInstanceOf[TextNode]) {
        val tn = node.asInstanceOf[TextNode]
        val text = tn.getWholeText()
        if (text.trim.length > 0) {
          val t2 = text + " " // re-insert separator
          val len = sb.length
          array.append(new NodePos(len, len + t2.length-1, tn))
          sb.append(t2)
        }
      }
    }
  }

}

object PackedText {

  /**
   * Maps a TextNode to start/end range in the combined text 
   * block thus supporting reverse lookup.
   * Performance considerations?
   */
  class NodePos(val start: Int, val end: Int, val node: TextNode) {
    override def toString = {
      val text = node.getWholeText
      getClass.getSimpleName() + s"($start,$end,$text)"
    }
  }

}