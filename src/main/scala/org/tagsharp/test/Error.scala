package org.tagsharp.test

import org.jsoup.nodes.Element
import org.tagsharp.jsoup.TextSpan

/**
 * When a Checkpoint Test is performed on an HTML document
 * any elements, attributes or text nodes that fail the test are flagged up.
 * They are captured as an Error.
 */
sealed abstract class Error

/**
 * Indicates that an error was found within an HTML element or attribute.
 */
case class ElementError(element: Element) extends Error

/**
 * Indicates that an error was found within a text node.
 */
case class TextError(textSpan: TextSpan) extends Error

/**
 * Indicates that an expected element or attribute was not found in the
 * HTML document. In this situation there is no error to highlight within
 * the document because nothing is there to highlight.
 * This is simply a marker to indicate that fact.
 */
case class AbsentError(reason: String) extends Error