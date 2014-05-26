package org.tagsharp.jsoup

import scala.collection.JavaConverters._
import org.jsoup.Jsoup
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.tagsharp.reporter.PageHighlightReporter
import org.tagsharp.jsoup.JSoupUtil.{appendAttribute, matchDocument, selectElements, updateStyle}


class TestJSoupUtil extends FlatSpec with ShouldMatchers {

  def failedToSplice = fail("Failed to splice TextNode")

  trait Fixture {

    val theMainHeading = "The Main Heading"
    val secondHeading = "Second Heading"

    val html =
      """
        |<html>
        |<head>
        |<title>Test Document</title>
        |<link href="style.css" rel="stylesheet" />
        |</head>
        |<body>
        |<h1 id="h1">The Main Heading</h1>
        |<h2 id="h2">Second Heading</h2>
        |<p foo-foo="baa">Follow this <a href="www.site.com">link</a> to learn more.</p>
        |<p foo-foo="baa">Then follow this <a style="bla" href="www.site2.com">that link</a> to learn more.</p>
        |</body>
        |</html>
      """.stripMargin

    val styleToFind = PageHighlightReporter.HighlightCssRule
    val doc = Jsoup.parse(html)
    val h1 = doc.getElementById("h1")
    val h2 = doc.getElementById("h2")
  }

  behavior of "JSoupUtil matchDocument"

  it should "matchDocument matches selectors" in new Fixture {
    def matches = matchDocument(doc)_
    matches("*[href]") should equal (true)
    matches("*[rel]") should equal (true)
  }

  it should "matchDocument does not match" in new Fixture {
    val matches = matchDocument(doc)_
    matches("*[src]") should equal (false)
  }


  behavior of "JSoupUtil selectElements"

  it should "selectElements selects href attributes" in new Fixture {
      selectElements(doc, "*[href]") should have size (3)
  }

  behavior of "JSoupUtil appendAttribute"

  it should "insert new attrMatch" in new Fixture {

    doc.select("*[foo-foo]").size() should equal (2) // pre-checks
    doc.select("a[foo-foo]").size() should equal (0)

    val elements = doc.select("a")
    elements should have size (2)
    elements.asScala.toList foreach (appendAttribute(_, "foo-foo", "bar"))
    doc.select("*[foo-foo=bar]").size should equal (2)
  }

  it should "append to existing attrMatch" in new Fixture {
    val elements = doc.select("p")
    elements should have size (2)
    elements.asScala.toList foreach (appendAttribute(_, "foo-foo", "bar"))
    val pelms = doc.select("p[foo-foo]").asScala.toList
    pelms.size should equal (2)
    pelms foreach (p => p.attr("foo-foo") should equal ("baa,bar"))
  }


  behavior of "JSoupUtil updateStyle"

  it should "updateStyle style matches" in new Fixture {

    val elements = doc.select("a")
    elements should have size (2)
    val elmsList = elements.asScala.toList
    elmsList foreach (updateStyle(_, styleToFind))
    val newHtml = doc.html()

    newHtml.split(styleToFind) should have length (3)
    newHtml.contains("bla; " + styleToFind) should equal (true) // one <a> contains style merges
  }

}
