package org.tagsharp.util

import util.matching.Regex

/**
 * Constructs a term string into a regex.
 * The regex constructed will match:
 *
 * <ul>
 *     <li>multiple words</li>
 *     <li>constrain to word boundaries (\b)</li>
 *     <li>case insensitive</li>
 *     <li>multi line</li>
 *     <li>replace spaces of the input with a whitespace match (\s+)</li>
 * </ul>
 */
object TermRegex  {

  def apply(string: String): Regex = make(string)

  def make(term: String):Regex = makeString(term).r

  def makeString(term: String) = (s"(?si)\\b$term\\b").replaceAll("""\s+""","""\\s+""")

}