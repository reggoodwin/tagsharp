package org.tagsharp.dsl

import scala.collection.JavaConverters._
import scala.annotation.tailrec
import util.matching.Regex
import org.jsoup.nodes.Element
import org.tagsharp.jsoup.{PackedText, TextSpan}


object RuleAST {

  sealed abstract class MatchResult()
  case class Pass() extends MatchResult
  case class Fail(failExamples: Seq[Any]) extends MatchResult

  /**
   * Represents the result of an internal computational step, 
   * e.g. predicate Boolean or countable Int.
   * This will eventually lead the creation of a final Result.
   */
  case class Step[T](step: T, examples: Seq[Any])

  class Rule(val selections: Selections, val predicate: Predicate) {

    def eval(element: Element):MatchResult = {
      val steps:Seq[Step[Boolean]] = selections.eval(element, predicate)
      val passed = steps.find(!_.step).isEmpty
      val parted = steps.partition(ir => ir.step)
      val truthies = for (ir <- parted._1; e <- ir.examples) yield (e)
      val falsies = for (ir <- parted._2; e <- ir.examples) yield (e)
      if (passed) Pass()
      else Fail(falsies)
    }

    override def toString = s"Rule($selections,$predicate)"
  }

  case class Selections(selections: Seq[Selection]) {
    def eval(e: Element, p: Predicate):Seq[Step[Boolean]] = {
      for {
        s <- selections
        step <- s.eval(e, p)
      } yield (step)
    }
  }

  abstract class Selection(selection: String) {
    def eval(e: Element, p: Predicate):Seq[Step[Boolean]]
  }

  case class ElementSelection(selection: String) extends Selection(selection) {
    def eval(e: Element, p: Predicate):Seq[Step[Boolean]] = {
      val elms:Seq[Element] = e.select(selection).asScala
      p.elementsTrue(elms)
    }
  }

  case class AttributeSelection(selection: String, attrName: String)
    extends Selection(selection)
  {
    def eval(e: Element, p: Predicate):Seq[Step[Boolean]] = {
      val elms:Seq[Element] = e.select(selection).asScala
      val attrs:Seq[Attribute] = elms.map(Attribute(attrName, _))
      p.attributesTrue(attrs)
    }
  }


  trait Predicate {
    def elementsTrue(elms: Seq[Element]): Seq[Step[Boolean]]
    def attributesTrue(attrs: Seq[Attribute]): Seq[Step[Boolean]]
  }

  case class Not(t: Predicate) extends Predicate {
    def elementsTrue(elms: Seq[Element]) = {
      t.elementsTrue(elms).map(s =>Step[Boolean](!s.step, s.examples))
    }
    def attributesTrue(attrs: Seq[Attribute]) = {
      t.attributesTrue(attrs).map(s =>Step[Boolean](!s.step, s.examples))
    }
  }

  abstract class MatchText(val text: String, val fn: String => Boolean) extends Predicate {
    def elementsTrue(elms: Seq[Element]) = {
      elms.map(e => Step[Boolean](fn(textOrData(e)), Seq(e)))
    }
    def attributesTrue(attrs: Seq[Attribute]) = {
      attrs.map(a => Step[Boolean](fn(a.value), Seq(a)))
    }
  }

  case class MatchRegex(regexStr: String) extends MatchText(
    regexStr:String, value => value.matches(regexStr)
  )

  case class IfMatchRegex(regexStr: String,
                           groupIndex: Int,
                           innerMatch: MatchRegex) extends MatchText(
    regexStr:String, value => {
      val TheRegex = new Regex(regexStr)
      val allMatches = TheRegex.findAllIn(value)
      val matches = allMatches.matchData map {
        m => m.group(groupIndex) match {
          case null => true // abort test: should it be true or false?
          case grpValue => innerMatch.fn(grpValue)
        }
      }
      matches.filter(_ == false).isEmpty
    }
  )

  /**
   * This has a number of differences from MatchRegex such that it requires
   * it's own matcher class.
   *
   * The typical usage for this is to check for one or more words
   * in paragraphs for tests of the prohibited words or case specifity kind.
   *
   * Match Reporting
   * ---------------
   *
   * This matcher matches on particular TextNodes within the given Elements
   * and reports those after a match rather than the enclosing Element.
   * This enables reporters to highlight specific words in paragraph text
   * rather than the entire paragraph.
   *
   * Problems
   * --------
   *
   * 1. Patterns that finish with characters like a dot "\." are not matching
   * because the pattern is being wrapped in \b boundaries.
   * Affixing with a ? solves this, but means that not all of the phrase
   * is consumed.
   *
   * 2. Match overflow - the concatenated text view means block content
   * is viewed as a single stream so characters from subsequent blocks
   * are being mistakenly consumed in tests for the previous block.
   *
   * 3. It is easy to forget which variation of this matcher overrides the
   * i flag and which uses no flags at all.
   *
   *
   * Performance Considerations
   * --------------------------
   *
   * !! This current version is not fast !!
   *
   * This uses an experimental utility class to produce a concatenated
   * view of all the text in each Element with reverse lookup into the
   * originating TextNode. Theoretically this will enable a large sequence
   * of patterns (i.e. words or terms) to be tested on a single string
   * which should be faster than testing all these terms across each
   * separate TextNode recursively as Jsoup does using
   * the :matches pseudo-selector. In practice
   * there is a cost to producing this view and performance is likely
   * to be slightly worse if there are not many terms being tested.
   *
   * @param patterns -
   *    a list of words to be found, expressed as regex patterns
   *    for maximum flexibility.Each pattern is modified by wrapping in
   *    "\b" boundary characters to avoid inter-word false positive matches.
   *
   * @param exactCase -
   *    if true this turns this matcher into a case specific matcher that
   *    checks if words in the text are of the required case. This variation
   *    requires two regex matches:
   *
   *    1. An initial case-insensitive match to find a word
   *    2. A case-sensitive match to ensure word has required case
   *
   *    Note: for case specific testing do not prepend patterns with
   *    the (?i) flag else case checking will have no effect.
   *
   */
  case class MatchWords(patterns: Seq[String], exactCase: Boolean = false) extends Predicate {

    patterns.map(p => new Regex(p)) // Validate patterns at compilation

    def elementsTrue(elms: Seq[Element]):Seq[Step[Boolean]] = {
      for {
        e <- elms
        pt = new PackedText(e) // TODO: consider caching?
        pattern <- patterns
        regex = mkRegex(pattern)
        textSpans = pt matchNode (regex)
        steps = textSpans match {
          case Nil => noMatch
          case list =>
            list.map(ts => {
              val matched =
                if (exactCase) ts.text.matches(pattern) && !isFalseMatch(ts) // Second case-sensitive match
                else !isFalseMatch(ts)
              Step[Boolean](matched, Seq(ts))
            })
        }
        step <- steps

      } yield (step)
    }

    /**
     * TODO:
     * This is primarily for testing URLs and emails in attributes.
      * Not happy that the pattern is modified by wrapping in .*? to match
     * any part of the String.
     */
    def attributesTrue(attrs: Seq[Attribute]):Seq[Step[Boolean]] = {
      for {
        a <- attrs
        attrValue = a.value
        pattern <- patterns
        pattern2 = ".*?" + """\b""" + pattern + """\b""" + ".*?" // Scala Regex tries to match entire string?
        regex = ((if (exactCase) "(?si)" else "") + pattern2).r
        matches = regex findAllMatchIn attrValue
        steps = if (matches.isEmpty) {
          noMatch
        } else {
          matches.map(m => {
            // Second case-sensitive match
            val matched = if (exactCase) attrValue.matches(pattern2) else true
            Step[Boolean](matched, Seq(a))
          })
        }
        step <- steps
      } yield (step)
    }

    def mkRegex(pattern: String):Regex = {
      val prefix = if (exactCase) "(?si)" else ""
      val bound = """\b"""
      (prefix + bound + pattern + bound).r
    }

    // This will be problematic for checkpoints testing lowercase URLs/emails
    def isFalseMatch(ts: TextSpan):Boolean = {
      false
      /*val s = ts.start
      val e = ts.end
      val r = ts.rewind(s)
      val a = ts.advance(e)
      if (r == s && a == e) false
      else {
        val t = ts.nodeText
        val border = "" + t.charAt(r) + t.charAt(a-1)
        !(border matches "\\s{2}")
      }*/
    }

    def noMatch:Seq[Step[Boolean]] = {
      if (exactCase) Seq(Step[Boolean](true, Nil))
      else Seq(Step[Boolean](false, Nil)) // avoid highlighting <body>
    }

  }


  case class LT() extends Order { def apply(x: Int, y:Int) = x < y }
  case class LTE() extends Order { def apply(x: Int, y:Int) = x <= y }
  case class GTE() extends Order { def apply(x: Int, y:Int) = x >= y }
  case class GT() extends Order { def apply(x: Int, y:Int) = x > y }
  case class EQ() extends Order { def apply(x: Int, y:Int) = x == y }

  case class Contain(countable: Countable, fn: Order, y:Int) extends Predicate {
    def elementsTrue(elms: Seq[Element]) = {
      elms.map(e => {
        val c = countable.count(e)
        Step[Boolean](fn(c.step, y), c.examples)
      })
    }
    def attributesTrue(attrs: Seq[Attribute]) = {
      attrs.map(a => {
        val c = countable.count(a)
        Step[Boolean](fn(c.step, y), c.examples)
      })
    }
  }

  case class Between(countable: Countable, x: Int, y: Int) extends Predicate {
    def between(c: Int) = c >= x && c <= y
    def elementsTrue(elms: Seq[Element]) = {
      elms.map(e => {
        val c = countable.count(e)
        Step[Boolean](between(c.step), c.examples)
      })
    }
    def attributesTrue(attrs: Seq[Attribute]) = {
      attrs.map(a => {
        val c = countable.count(a)
        Step[Boolean](between(c.step), c.examples)
      })
    }
  }

  case class Occur(order: Order, y: Int) extends Predicate {
    def elementsTrue(elms: Seq[Element]) = {
      val t = order(elms.size, y)
      if (elms.isEmpty) Seq(Step[Boolean](t, Nil))
      else elms.map(e => Step[Boolean](t, Seq(e)))
    }
    def attributesTrue(attrs: Seq[Attribute]) = {
      if (attrs.isEmpty) Nil //fails a Not inversion: Seq(Step(true, Nil))
      else attrs.map(a => {
        val c = if (a.hasValue) 1 else 0
        Step[Boolean](order(c, y), Seq(a))
      })
    }
  }

  trait Countable {
    def count(e: Element):Step[Int]
    def count(a: Attribute):Step[Int]
  }

  trait Order {
    def apply(x: Int, y:Int): Boolean
  }


  case class Words() extends Countable {
    def words(t: String):Seq[String] = if (t.isEmpty) Seq() else t.split("\\s+").toSeq
    def count(e: Element) = Step[Int](words(textOrData(e)).size, Seq(e))
    def count(a: Attribute) = Step[Int](words(a.value.trim).size, Seq(a))
  }

  case class Characters() extends Countable {
    def count(e: Element) = Step[Int](textOrData(e).size, Seq(e))
    def count(a: Attribute) = Step[Int](a.value.trim.size, Seq(a))
  }

  /**
   * For sub-selects that apply to attributes and text of the subject
   * under test because it is the subject returned as Example.
   */
  abstract sealed class CssContain(selection: String) extends Predicate with Countable {

    def elementsTrue(elements: Seq[Element]) = {
      elements.map(e => Step[Boolean](!select(e).isEmpty, Seq(e)))
    }

    def count(elm: Element) = Step[Int](select(elm).size, Seq(elm))

    def select(elm: Element):Seq[Element] = elm.select(selection).asScala
    
    def count(attr: Attribute) = notForAttributes
    def attributesTrue(attributes: Seq[Attribute]) = notForAttributes
  }

  case class ContainSelection(selection: String) extends CssContain(selection)

  /**
   * Overrides ContainSelection to produce examples on the 
   * sub-selection rather than the elements being tested.
   */
  case class ContainSubSelection(selection: String) extends CssContain(selection) {
    
    override def elementsTrue(elms: Seq[Element]) = {
      elms.map(e => {
        val subselect = e.select(selection).asScala
        Step[Boolean](!subselect.isEmpty, subselect)
      })
    }

    override def count(elm: Element) = {
      val sel = elm.select(selection).asScala
      Step[Int](sel.size, sel)
    }
  }

  case class ContainAttribute(attrCss: String) extends CssContain("[" + attrCss + "]") {

    // Should only select on the current Element.
    // This is intended to avoid unintended selection on grandchildren of the Element.
    // E.g. if searching for a [bgColor] on a <table>, this should not return
    // a <td> if the bgColor were to be found on the <td>.
    // (Actually it might be more useful in this instance if it did, but you get
    // the idea.)

    override def select(elm: Element) = {
      if (elm.attributes().get(attrCss).isEmpty) Nil
      else Seq(elm)
    }

    /*
    // this won't work for expressions like [attrName=value]
    def attribute(e: Element):Option[Attribute] = {
      val v = e.attributes().get(attrCss)
      if (v.isEmpty) None else Some(Attribute(attrCss, e))
    }

    // unused?
    override def elementsTrue(elements: Seq[Element]) = {
      elements.map(
        attribute(_) match {
          case Some(a) => Step(false, Seq(a))
          case None => Step(true, Nil)
        }
      )
    }
    */

  }

  /**
   * Slightly devious way of getting text within an element or the data
   * within elements like <style> and <script> without having to test by
   * element name. If it were possible for an element to have both text AND data
   * then of course this would not work properly and require splitting out.
   */
  def textOrData(e: Element):String = {
    e.text().trim + e.data()
  }

  // Code smell to fix ...
  def notForAttributes = throw new IllegalStateException("Not supported for attributes")

}
