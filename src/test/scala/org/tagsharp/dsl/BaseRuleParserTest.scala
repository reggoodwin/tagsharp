package org.tagsharp.dsl

import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.tagsharp.dsl.RuleAST.{Fail, Pass, MatchResult, Rule}
import org.tagsharp.dsl.RuleParsers.RuleParser


trait BaseRuleParserTest extends FlatSpec with ShouldMatchers {

  val parser = new RuleParser()

  def parse(terms: String):Rule = {
    val parseResult = parser.parseRule(terms)
    if (parseResult.isEmpty) fail(parseResult.toString)
    else parseResult.get
  }

  def flop(result: MatchResult) = fail("Unexpected result:\n" + result.toString)

  def toPass(terms: String)(test: Rule => Unit):Unit = testRule("PASS", terms, test)
  def toFail(terms: String)(test: Rule => Unit):Unit = testRule("FAIL", terms, test)

  def testRule(passFail: String, terms: String, test: Rule => Unit):Unit = {
    it should (passFail + " " + terms) in {
      val rule = parse(terms)
      test(rule)
    }
  }


  trait Eval {
    def doc: Document
    def eval(terms: String) = parse(terms).eval(doc)

    def byId(idSelector:String) = doc.select("#" + idSelector).first
    def first(selector:String) = doc.select(selector).first

    def html = doc.select("html").first
    def head = doc.select("head").first
    def body = doc.select("body").first
  }

  def withTerms(terms: String)(fn: String => Unit):Unit = {
    val oneLineTerms = terms.replaceAll("\n", " ").trim
    it should s"eval: $oneLineTerms" in {
      fn(terms)
    }
  }

  class EmptyDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |  <title>Title</title>
        |</head>
        |<body>
        |  A mostly empty document to verify non-failure of "not present" elements.
        |</body>
        |</html>
      """.stripMargin)
  }

  class GoodDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html lang="en-us">
        |<head>
        |  <title>Company | A Reasonable Length Page Title</title>
        |  <meta name="description" content="about the page" />
        |</head>
        |<body>
        |<h1>Page Heading</h1>
        |<p>An <a href="http://www.site.com/file">anchor with link text</a>.</p>
        |<p>An <img alt="alt text" src="http://www.site.com/image" height="10" width="10"> image with root referential src.</p>
        |<p>An image map <map><area alt="fragment" shape="rect" coords="0,0,100,100"></area></map></p>
        |<p>View the <a href="http://www/site.com/sitemap.xml">sitemap link</a>.</p>
        |<iframe title="title"></iframe>
        |</body>
        |</html>
      """.stripMargin
    )
    val h1 = first("h1")
  }

  class BadDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |  <title>Company</title>
        |  <meta id="meta0" name="description" content="  " />
        |</head>
        |<body>
        |<p>Page With No Headings</p>
        |<p>Paragraph with a <b>bold</b> and <i>italic</i> tag.</p>
        |<p>Then an anchor which reads <a id="a0"> click here <strong>with &gt;&gt; arrows</strong></a>.</p>
        |<p>Then an <a id="a1">anchor with a very long link title exceeding the maximum number of characters expected for a link title</a>.</p>
        |<p>Then an anchor with no link text: <a id="a2" href="http://www.site.com"></a>.</p>
        |<p>Then an image with relative src <img id="img0" src="/image" />.</p>
        |<p><img id="img1" src="/image" alt="alt tag" />.</p>
        |<p>A relative link: <a id="a3" href="/somepage">some page mentioned here</a>.</p>
        |<p>Link with url in title: <a id="a4" href="http://www.site.com">http://www.site.com</a>.</p>
        |<iframe id="iframe0"></iframe>
        |<div id="footer">No copyright statement</div>
        |<script type="text/javascript">No tracking code</script>
        |</body>
        |</html>
      """.stripMargin
    )
    val title = first("title")
    val meta0 = byId("meta0")
    val b = first("b")
    val i = first("i")
    val a0 = byId("a0")
    val a1 = byId("a1")
    val a2 = byId("a2")
    val a3 = byId("a3")
    val a4 = byId("a4")
    val img0 = byId("img0")
    val img1 = byId("img1")
    val div0 = byId("div0")
    val div1 = byId("div1")
    val div2 = byId("div2")
    val div3 = byId("div3")
    val divFooter = byId("footer")
    val iframe0 = byId("iframe0")
  }

  class Doc3 extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |  <title>Page Title</title>
        |</head>
        |<body>
        |<h2 id="h2">Page With No H1 Headings</h2>
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
    val title = first("title")
    val h2 = first("h2")
  }

  class Doc4 extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Page Title</title></head>
        |<body>
        |<h2>Page With No H1 Headings</h2>
        |<p>A page with <u>underlining</u> added.</p>
        |<p>Then an <a id="a0">anchor <strong>with >> arrows</strong></a>.</p>
        |<p><img id="img0" alt="one-word-only" src="/image" />.</p>
        |<form>
        | <input type="image" id="input0" /> no alt attribute!
        | <input type="text" name="textfield" />
        |</form>
        |<embed></embed> <!-- missing noembed -->
        |</body>
        |</html>
      """.stripMargin
    )

    val u = first("u")
    val a0 = byId("a0")
    val a1 = byId("a1")
    val a2 = byId("a2")
    val img0 = byId("img0")
    val input0 = byId("input0")
  }

}
