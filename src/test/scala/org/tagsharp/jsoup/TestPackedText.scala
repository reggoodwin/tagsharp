package org.tagsharp.jsoup

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.jsoup.Jsoup
import org.tagsharp.util.CustomMatchers


class TestPackedText extends FlatSpec with ShouldMatchers with CustomMatchers {

  trait Fixture {
    val html =
      """
        |<html><head><title>Test Doc</title></head>
        |<body>
        |<h1>Some Heading</h1>
        |<div id="content">
        | Starting at the beginning
        | <other>There is a paragraph with a <a>hyperlink</a> and <em>emphasised text</em> to consider.</other>
        | <other>Later, there is another <strong>paragraph</string> and more.</other>
        |</div>
        |</body>
        |</html>
      """.stripMargin

    val doc = Jsoup.parse(html)
    val packedText = new PackedText(doc.body())
  }

  def failed(nodes: List[TextSpan]) = fail("Did not find expected nodes in: " + nodes)

  behavior of "PackedText"

  it should "find single nodes (whole match only)" in new Fixture {
    packedText matchNode ("""(?si)\bstarting\b""".r) match {
      case List(TextSpan(3, 11, node)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bbeginning\b""".r) match {
      case List(TextSpan(19, 28, node)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bhyperlink\b""".r) match {
      case List(TextSpan(0, 9, node)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bparagraph\b""".r) match {
      case List(
        TextSpan(11, 20, _),
        TextSpan(0, 9, _)
      ) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\band\b""".r) match {
      case List(
        TextSpan(1, 4, _),
        TextSpan(1, 4, _)
      ) =>
      case f => failed(f)
    }

  }

  it should "find multi-word terms (whole match only)" in new Fixture {

    packedText matchNode ("""(?si)\bsome\s+heading\b""".r) match {
      case List(TextSpan(0, 12, _)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bparagraph\s+with\s+a\b""".r) match {
      case List(TextSpan(11, 27, _)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bsome\s+heading\b""".r) match {
      case List(TextSpan(0, 12, _)) =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bto\s+consider\b""".r) match {
      case List(TextSpan(1, 12, _)) =>
      case f => failed(f)
    }

  }

  it should "not find multi-word terms across nodes (whole match only)" in new Fixture {

    packedText matchNode ("""(?si)\bheading\s*starting\b""".r) match {
      case Nil =>
      case f => failed(f)
    }

    packedText matchNode ("""(?si)\bbeginning\s*there\b""".r) match {
      case Nil =>
      case f => failed(f)
    }

  }

}
