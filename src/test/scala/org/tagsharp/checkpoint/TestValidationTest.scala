package org.tagsharp.checkpoint

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.tagsharp.test.{Fail, Pass}


class TestValidationTest extends FlatSpec with ShouldMatchers {

  behavior of "ValidationTest"


  val doc1 = Jsoup.parse(
    """
      |<html lang="en-us">
      |<head>
      |  <title>Company | An extremely long page title that goes into too much detail</title>
      |  <meta name="description" content="about the page" />
      |</head>
      |<body>
      |<h1>Page Heading</h1>
      |<p>An <a href="http://www.site.com/file">anchor with link text</a>.</p>
      |<p>An <img alt="alt text" src="http://www.site.com/image"> image with root referential src.</p>
      |<p>An image map <map><area alt="fragment" shape="rect" coords="0,0,100,100"></area></map></p>
      |</body>
      |</html>
    """.stripMargin
  )

  val doc2 = Jsoup.parse(
    """
      |<html>
      |<head>
      |  <title>Company</title>
      |  <meta name="description" /> <!-- content attribute missing -->
      |</head>
      |<body>
      |<p>Page With No Headings</p>
      |<p>Paragraph with a <b>bold</b> and <i>italic</i> tag.</p>
      |<p>Then a <blink>blinking paragraph</blink> as well.</p>
      |<p>Then an anchor which reads <a> click here <strong>with &gt;&gt; arrows</strong></a>.</p>
      |<p>Then an <a>anchor with a very long link title exceeding the maximum number of characters expected for a link title</a>.</p>
      |<p>Then an anchor with no link text: <a href="http://www.site.com"></a>.</p>
      |<p>Then an image with relative src <img src="/image" />.</p>
      |<p>Then an anchor and image <a href="http://www.site.com"><img src="/image" alt="   "/></a>.</p>
      |<p>A image map <map><area shape="rect" coords="0,0,100,100"></area></map></p>
      |<p><img src="/image" alt="Image tag with an overly long explanation in the alt attribute of the tag that could be summarised in fewer words" />.</p>
      |</body>
      |</html>
    """.stripMargin
  )

  val doc3 = Jsoup.parse(
    """
      |<html>
      |<head>
      |  <title>Page Title</title>
      |</head>
      |<body>
      |<h2>Page With No H1 Headings</h2>
      |<form>
      | <input type="image" alt="alt description" />
      | <input type="text" name="textfield" />
      |</form>
      |<p>
      | <a href="somefile.pdf" target="_blank">a PDF file</a>
      | <a href="somefile.xls" target="_blank">an Excel file</a>
      |</p>
      |<embed>
      |  <noembed>Your browser does not support this</noembed>
      |</embed>
      |</body>
      |</html>
    """.stripMargin
  )

  val doc4 = Jsoup.parse(
    """
      |<html>
      |<head><title>Page Title</title></head>
      |<body>
      |<h2>Page With No H1 Headings</h2>
      |<p>A page with <u>underlining</u> added.</p>
      |<p>Then an <a>anchor <strong>with >> arrows</strong></a>.</p>
      |<p><img alt="one-word-only" src="/image" />.</p>
      |<form>
      | <input type="image" /> no alt attrMatch!
      | <input type="text" name="textfield" />
      |</form>
      |<p>
      | <a href="somefile.pdf">a pdf file</a> <!-- missing target attribute -->
      | <a href="somefile.xls">an Excel file</a>
      |</p>
      |<embed></embed> <!-- missing noembed -->
      |</body>
      |</html>
    """.stripMargin
  )

  val doc5 = Jsoup.parse(
    """
      |<html>
      |<head>
      |  <title>Page Title</title>
      |</head>
      |<body>
      |<h2>Page With No H1 Headings</h2>
      |<table class="red">
      |  <tr><th>heading</th></tr>
      |  <tr><td>cell</td></tr>
      |</table>
      |</body>
      |</html>
    """.stripMargin
  )

  val doc6 = Jsoup.parse(
    """
      |<html>
      |<head>
      |    <title>Page Title</title>
      |</head>
      |<body>
      |<h2>Page With No H1 Headings</h2>
      |<table>
      |  <tr><td bgcolor="red">fake heading row</td></tr>
      |  <tr><td>cell</td></tr>
      |</table>
      |</body>
      |</html>
    """.stripMargin
  )

  /* = = = = =  Utility  = = = = = = */


  def shouldPass(doc: Document, terms: String) = {
    val test = new ValidationTest(terms)
    test.apply(doc) match {
      case Pass() =>
      case f: Fail => fail("Should have passed: " + f)
    }
  }
  def shouldFail(doc: Document, terms: String) = {
    val test = new ValidationTest(terms)
    val result = test.apply(doc)
    result match {
      case Pass() => fail("Should have flop: " + result + ", rule: " + test.rule)
      case _ =>
    }
  }

  def passFailTest(passingDoc: Document, failingDoc: Document, terms: String) ={
    shouldPass(passingDoc, terms)
    shouldFail(failingDoc, terms)
  }

}
