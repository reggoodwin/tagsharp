package org.tagsharp.reporter

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.jsoup.Jsoup
import org.tagsharp.reporter.ErrorTagger._
import org.tagsharp.util.TestingUtil._
import org.tagsharp.test.{TextError, Test, ElementError}
import org.tagsharp.util.Span
import org.tagsharp.jsoup.TextSpan
import org.tagsharp.checkpoint.Checkpoint


class TestErrorTagger extends FlatSpec with ShouldMatchers {

  behavior of "ErrorTagger"

  trait TagElementFixture {

    val existingValue = "1234"

    val html =
      """
        |<html>
        |<head><title>Test</title></head>
        |<body>
        |<h1 id="h1">Main Heading</h1>
        |<p id="2">A paragraph of <strong id="s" data-error="1234">text</strong>
        |and a <a id="a">hyperlink</a> too.</p>
        |</body>
        |</html>
      """.stripMargin

    val doc = Jsoup.parse(html)

    val checkpoint = Checkpoint("1234", "Mock", mockTest("Mock"))
  }

  it should "add tag attrMatch to h1" in new TagElementFixture {
    val h1 = doc.getElementById("h1")
    new ErrorTagger().tag(checkpoint, ElementError(h1))
    h1.attr(ErrorAttribute) should equal (checkpoint.id)
  }

  it should "add tag attrMatch to anchor" in new TagElementFixture {
    val anchor = doc.getElementById("a")
    new ErrorTagger().tag(checkpoint, ElementError(anchor))
    anchor.attr(ErrorAttribute) should equal (checkpoint.id)
  }

  it should "add tag attrMatch to wrapped h1" in new TagElementFixture {
    def tagger = new ErrorTagger(wrappingTag = ErrorTagWrapper, wrapElements = true)
    val h1 = doc.getElementById("h1")
    tagger.tag(checkpoint, ElementError(h1))
    h1.parent.attr(ErrorAttribute) should equal (checkpoint.id)
    h1.parent.nodeName should equal (ErrorTag)
  }

  it should "add tag attrMatch to wrapped anchor" in new TagElementFixture {
    def tagger = new ErrorTagger(wrappingTag = ErrorTagWrapper, wrapElements = true)
    val anchor = doc.getElementById("a")
    tagger.tag(checkpoint, ElementError(anchor))
    anchor.parent.attr(ErrorAttribute) should equal (checkpoint.id)
    anchor.parent.nodeName should equal (ErrorTag)
  }

  it should "append tag attrMatch" in new TagElementFixture {
    val strong = doc.getElementById("s")
    new ErrorTagger().tag(checkpoint, ElementError(strong))
    strong.attr(ErrorAttribute) should equal (existingValue + "," + checkpoint.id)
  }

  it should "not append tag attrMatch to new wrapping tag" in new TagElementFixture {
    def tagger = new ErrorTagger(wrappingTag = ErrorTagWrapper, wrapElements = true)
    val strong = doc.getElementById("s")
    tagger.tag(checkpoint, ElementError(strong))
    strong.attr(ErrorAttribute) should equal (existingValue)
    strong.parent.attr(ErrorAttribute) should equal (checkpoint.id)
    strong.parent.nodeName should equal (ErrorTag)
  }

  behavior of "ErrorTagger text errors"

  //
  // A range of text highlighting tests should cover these scenarios:
  //
  // Location
  // ========
  // These tests prove that highlighting can be applied to any
  // part of a TextNode whether that be start, middle or end.
  //
  // * entire text in TextNode error
  // * a term within the text of a single TextNode
  // * start and end term error in single TextNode
  // * multiple middling terms in single TextNode
  //
  // Multi-Node
  // ==========
  // These tests prove that highlighting works when the parent node
  // contains multiple TextNode (including the one being tested) and that
  // the that nodes/elements are not corrupted by the highlighting of this textNode.
  //
  // * ???
  //
  // Merge Conflict
  // ==============
  // These tests prove that merge conflicts are fully resolved:
  //
  // * Two errors exact same term
  // * Two errors, one error on a word fully contained by match in the that
  // * Two errors, each overlapping a word from the that
  // * Two adjacent errors (to prove that a merge was not applied)
  //
  // Hybrid tests:
  //
  // * A mixed scenario of location based errors and merge conflicts
  //

  /**
   * @see TestTextSpan
   */
  trait MergeTestFixture {

    // s0  [0 ... 4]
    // s1         [5 ... 10]
    // s2               [8 ... 12]
    // s3                     [10 ... 14]
    // s4                               [15 ... 20]

    val checkpoint1 = Checkpoint("1234", "Mock1", mockTest("Test1"))
    val checkpoint2 = Checkpoint("4321", "Mock2", mockTest("Test2"))

    val s0 = Span(0, 4)
    val s1 = Span(5, 10)
    val s2 = Span(8, 12)
    val s3 = Span(10, 14)
    val s4 = Span(15, 20)

    val t0 = (checkpoint1, s0)
    val t1 = (checkpoint1, s1)
    val t2 = (checkpoint1, s2)
    val t3 = (checkpoint1, s3)
    val t4 = (checkpoint2, s4)

    val t01 = (checkpoint2, s0)
    val t21 = (checkpoint2, s2)
    val t012 = List( (List(checkpoint1),s0), (List(checkpoint1),Span(s1.start,s2.end)))
    val t123 = List( (List(checkpoint1),Span(s1.start,s3.end)) )
    val t0123 = List( (List(checkpoint1),s0), (List(checkpoint1),Span(s1.start,s3.end)) )
    val t1234 = List( (List(checkpoint1),Span(s1.start,s3.end)), (List(checkpoint2),s4) )
    val t01234 = List( (List(checkpoint1),s0), (List(checkpoint1),Span(s1.start,s3.end)), (List(checkpoint2),s4) )
    val t001 = List( (List(checkpoint2, checkpoint1),s0) )
    val t121 = List( (List(checkpoint2, checkpoint1), Span(s1.start, s2.end)) )

    val tagger = new ErrorTagger
  }

  it should "merge pairs affecting single Test" in new MergeTestFixture {
    tagger mergedPairs (List(t0)) should equal (List((List(checkpoint1), s0)))
    tagger mergedPairs (List(t1)) should equal (List((List(checkpoint1), s1)))
    tagger mergedPairs (List(t0, t1)) should equal (
      List((List(checkpoint1), s0), (List(checkpoint1), s1))
    )
    tagger mergedPairs (List(t1, t2)) should equal (
      List((List(checkpoint1), Span(s1.start, s2.end)))
    )
    tagger mergedPairs (List(t0, t1, t2)) should equal (t012)
    tagger mergedPairs (List(t0, t1, t2, t3)) should equal (t0123)
    tagger mergedPairs (List(t3, t2, t1, t0)) should equal (t0123)
    tagger mergedPairs (List(t2, t3, t1)) should equal (t123)
    tagger mergedPairs (List(t3, t2, t1)) should equal (t123)
    tagger mergedPairs (List(t3, t2, t4, t1)) should equal (t1234)
    tagger mergedPairs (List(t3, t2, t0, t4, t1)) should equal (t01234)
    tagger mergedPairs (List(t0, t2, t3, t4, t1)) should equal (t01234)
    tagger mergedPairs (List(t1, t4, t0, t2, t3)) should equal (t01234)
  }

  it should "merge pairs affecting multiple Tests" in new MergeTestFixture {
    tagger mergedPairs (List(t0, t01)) should equal (t001)
    tagger mergedPairs (List(t1, t21)) should equal (t121)
  }

  trait TagTextNodeFixture {

    val html =
      """
        |<html>
        |<head><title>Test</head>
        |<body>
        |<p id="p1">A paragraph of text made of a single TextNode.</p>
        |<p id="p2">A paragraph of <strong>text</strong> made of <em>multiple</em> TextNode.</p>
        |</body>
        |</html>
      """.stripMargin

    val doc = Jsoup.parse(html)
    val p1 = doc.getElementById("p1")
    val p2 = doc.getElementById("p2")

    val checkpoint1 = Checkpoint("1234", "Mock1", mockTest("Test1"))
    val tagger = new ErrorTagger
  }

  it should "tag 1 error in 1 TextNode" in new TagTextNodeFixture {
    val tn = p1.textNodes().get(0)
    val errors = List(
      (List(checkpoint1), Span(0, tn.getWholeText.length))
    )
    tagger.applyTextNodeTags(tn, errors)
    p1.html should (include (">A paragraph of text made of a single TextNode.</span>"))
  }

  it should "tag 2 edged matches" in new TagTextNodeFixture {
    val errors = List(
      (List(checkpoint1), Span(0, 11)), // 'A paragraph'
      (List(checkpoint1), Span(37, 46)) // 'TextNode.'
    )
    tagger.applyTextNodeTags(p1.textNodes().get(0), errors)
    p1.html should (
      include (">A paragraph</span> of text made of a single ") and
        include (">TextNode.</span>")
      )
  }

  it should "tag 2 midway matches" in new TagTextNodeFixture {
    val errors = List(
      (List(checkpoint1), Span(2, 11)), // 'paragraph'
      (List(checkpoint1), Span(15, 19)) // 'text'
    )
    tagger.applyTextNodeTags(p1.textNodes().get(0), errors)
    p1.html should (
      include (">paragraph</span>") and
        include (">text</span> made of a single TextNode.")
      )
  }

  it should "tag first TextNode in node with multiple TextNode" in new TagTextNodeFixture {
    tagger.applyTextNodeTags(
      p2.textNodes().get(0),
      List((List(checkpoint1), Span(2, 14))) // 'paragraph of'
    )
    p2.html should (
      include ("A <span") and
        include (">paragraph of</span> <strong>text</strong> made of <em>multiple</em> TextNode.")
      )
  }

  it should "tag middle TextNode in multi TextNode node" in new TagTextNodeFixture {
    tagger.applyTextNodeTags(
      p2.textNodes().get(1),
      List((List(checkpoint1), Span(1, 8))) // 'made of'
    )
    p2.html should (
      include ("A paragraph of <strong>text</strong> <span") and
        include (">made of</span> <em>multiple</em> TextNode.")
      )
  }

  it should "tag last TextNode in multi TextNode node" in new TagTextNodeFixture {
    tagger.applyTextNodeTags(
      p2.textNodes().get(2),
      List((List(checkpoint1), Span(1, 9))) // 'TextNode'
    )
    p2.html should (
      include ("A paragraph of <strong>text</strong> made of <em>multiple</em> <span") and
        include (">TextNode</span>.")
      )
  }


  trait FullTextNodeTagFixture {

    val html =
      """
        |<html>
        |<head><title>Test</title></head>
        |<body>
        |<h1 id="h1">Main Heading</h1>
        |<p id="p1">A paragraph of <strong id="s">text</strong> on the page.</p>
        |</body>
        |</html>
      """.stripMargin

    val doc = Jsoup.parse(html)
    val h1 = doc.getElementById("h1")
    val p = doc.getElementById("p1")
    val strong = doc.getElementById("s")
    val checkpoint1 = Checkpoint("1234", "Mock1", mockTest("Test1"))
    val tagger = new ErrorTagger
  }

  it should "tag single text error" in new FullTextNodeTagFixture {
    val tn = p.textNodes().get(0)
    val errors: List[(Checkpoint, TextError)] = List(
      (checkpoint1, TextError(TextSpan(2, 11, tn))) // 'paragraph'
    )
    tagger.tag(errors)
    p.html should (
      include ("A <span") and
      include (">paragraph</span> of <strong id=\"s\">text</strong> on the page.") and
      include (checkpoint1.id)
    )
  }


}
