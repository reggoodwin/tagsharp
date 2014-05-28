package org.tagsharp.jsoup

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.language.postfixOps
import org.jsoup.nodes.{Node, TextNode, Element, Document}
import org.jsoup.select.{NodeTraversor, NodeVisitor}


object JSoupUtil {

  def matchDocument(doc: Document)(selector: String):Boolean = {
    selectElements(doc, selector).size > 0
  }

  def selectElement(element: Element, selector: String):Option[Element] = {
    val elements = selectElements(element, selector)
    if (elements.isEmpty) None else Some(elements.head)
  }

  def selectElements(element: Element, selector: String):List[Element] = {
    element.select(selector).asScala.toList
  }

  def extractTextNodes(elements: List[Element]):List[TextNode] = {
    for {
      elm <- elements
      nodes = extractTextNodes(elm)
      node <- nodes
    } yield (node)
  }

  def extractTextNodes(element: Element):List[TextNode] = {
    element.textNodes().asScala.toList
  }

  def extractDocumentTextNodes(doc: Document):Seq[TextNode] = {
    val buffer = new ArrayBuffer[TextNode]
    new NodeTraversor(new NodeVisitor() {
      def head(node: Node, p2: Int) {}
      def tail(node: Node, p2: Int) {
        if (node.isInstanceOf[TextNode]) {
          buffer.append(node.asInstanceOf[TextNode])
        }
      }
    }).traverse(doc)
    buffer
  }

  def appendAttribute(node: Node, attrName: String, attrValue: String) {
      appendAttribute(node, attrName, attrValue, ",")
  }

  /**
   * Inserts or appends to an existing attrMatch in the given Node.
   */
  def appendAttribute(node: Node, attrName: String, attrValue: String, separator: String) {

    def merge(v1:String, v2:String) =
      if (v1 isEmpty) v2
      else v1 + (if (v1 endsWith separator) "" else separator) + v2

    val oldVal = node.attr(attrName).trim
    node.attr(attrName, merge(oldVal, attrValue))
  }

  /**
   * Adds or appends a class attribute to the given node
   */
  def appendClass(node: Node, extraClass: String) {
    def merge(s:String, s2:String) = if (s isEmpty) s2 else s + " " + s2
    val classStyle = node.attr("class").trim
    node.attr("class", merge(classStyle, extraClass))
  }

}