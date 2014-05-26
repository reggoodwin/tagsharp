package org.tagsharp.dsl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.jsoup.Jsoup
import org.tagsharp.jsoup.TextSpan
import org.tagsharp.dsl.RuleAST.{MatchWords, Step, IfMatchRegex, MatchRegex}


class TestRuleAST extends FlatSpec with ShouldMatchers {

  behavior of "MatchRegexFine"

  class TextDoc {

    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head>
        |<body>
        |<p>A paragraph with a <a>link</a> included in the PARAGRAPH.</p>
        |</body>
        |</html>
      """.stripMargin)

    val body = doc.body()
    val p = doc.select("p").first
    val pt = p.textNodes().get(0)
    val pt1 = p.textNodes().get(1)
    val a = doc.select("a").first
    val at = a.textNodes().get(0)
    val elms = Seq(body)
  }

  it should "match outer text" in new TextDoc {
    MatchWords(Seq("paragraph")).elementsTrue(elms) should equal (
      Seq(Step(true, Seq(TextSpan(2, 11, pt))))
    )
  }

  it should "not match outer text" in new TextDoc {
    MatchWords(Seq("pharagraph")).elementsTrue(elms) should equal (
      Seq(Step(false, Nil))
    )
    MatchWords(Seq("para")).elementsTrue(elms) should equal (
      Seq(Step(false, Nil))
    )
  }

  it should "match inner text" in new TextDoc {
    MatchWords(Seq("link")).elementsTrue(elms) should equal (
      Seq(Step(true, Seq(TextSpan(0, 4, at))))
    )
  }

  it should "match outer and inner text" in new TextDoc {
    MatchWords(Seq("paragraph", "link")).elementsTrue(elms) should equal (
      Seq(
        Step(true, Seq(TextSpan(2, 11, pt))),
        Step(true, Seq(TextSpan(0, 4, at)))
      )
    )
  }

  it should "match one out of two" in new TextDoc {
    MatchWords(Seq("pharagraph", "link")).elementsTrue(elms) should equal (
      Seq(Step(false, Nil), Step(true, Seq(TextSpan(0, 4, at))))
    )
    MatchWords(Seq("paragraph", "lynk")).elementsTrue(elms) should equal (
      Seq(Step(true, Seq(TextSpan(2, 11, pt))), Step(false, Nil))
    )
  }

  it should "match case specific" in new TextDoc {
    MatchWords(Seq("paragraph"), true).elementsTrue(elms) should equal (
      Seq(
        Step(true, Seq(TextSpan(2, 11, pt))),
        Step(false, Seq(TextSpan(17, 26, pt1)))
      )
    )
  }

  it should "match case specific 2" in new TextDoc {
    MatchWords(Seq("PARAGRAPH"), true).elementsTrue(elms) should equal (
      Seq(
        Step(false, Seq(TextSpan(2, 11, pt))),
        Step(true, Seq(TextSpan(17, 26, pt1)))
      )
    )
  }

  it should "not match case specific" in new TextDoc {
    MatchWords(Seq("para", "PARA", "graph", "GRAPH"), true).elementsTrue(elms) should equal (
      Seq(Step(true,Nil), Step(true,Nil), Step(true,Nil), Step(true,Nil))
    )
  }

  class FalsePosWordsDoc {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head>
        |<body>
        |<p>A paragraph-like with a <a>link</a> included in the PARAGRAPH-LIKE.</p>
        |</body>
        |</html>
      """.stripMargin)
    val elms = Seq(doc.body())
  }

  /*it should "not match false positives" in new FalsePosWordsDoc {
    MatchWords(Seq("paragraph")).elementsTrue(elms) should equal (
      Seq(Step(false, Nil))
    )
    MatchWords(Seq("paragraph"), true).elementsTrue(elms) should equal (
      Seq(Step(false, Nil))
    )
  } */

  behavior of "IfMatchRegex"

  trait FontMatcher {

    // The regex to parse a CSS name/value pair is problematic because
    // the value is optionally within quotes.
    // * If value has no quotes then the end of value is a space/semicolon/brace.
    // * If value is inside quotes then spaces are allowed!
    // Todo: Comments should also be ignored /* ... */
    // Long term a CSS parser would do the job better!

    val subMatch = IfMatchRegex(
      """font-family\s*:\s*(["']([^"']*)["']|[^\s;<>{}"'}]+)""", 1,
      MatchRegex("""Arial|Helvetica|["']Sans Serif["']""")
    )

  }

  trait StyleDoc {
    val html: String
    def doc = Jsoup.parse(html)
    lazy val style0 = doc.select("style").first
    lazy val elms = Seq(style0)
  }

  // outer match TRUE inner match TRUE

  it should "find font-family attribute and font" in {
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: Arial; </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-size: 1em;font-family: Arial; </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-size: 1em;font-family:Arial; </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: Helvetica </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: "Sans Serif" </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: 'Sans Serif' </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
  }

  // outer match TRUE inner match FALSE

  it should "find font-family attribute but not font" in {
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: Other; </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(false, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: Other</style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(false, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: 'Sans-Serif' </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(false, Seq(style0))))
    }
  }

  // outer match FALSE .. should abort inner match

  it should "not find font-family value" in {
    new FontMatcher with StyleDoc {
      val html = """<style>/* no font family */</style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: ;</style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family: ;Arial </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
    new FontMatcher with StyleDoc {
      val html = """<style> font-family Arial; </style>"""
      subMatch.elementsTrue(elms) should equal (Seq(Step(true, Seq(style0))))
    }
  }

}
