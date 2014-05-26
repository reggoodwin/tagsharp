package org.tagsharp.checkpoint

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.tagsharp.test.TestRunner
import org.tagsharp.checkpoint.Highlightable._
import org.tagsharp.reporter.{PageSourceHighlightReporter, PageHighlightReporter}
import org.tagsharp.util.TestingUtil.{readFileContents, SamplePagesDirectory}


class TestTestRunner extends FlatSpec with ShouldMatchers {
  
  trait Fixture {
    def html: String
    val doc = Jsoup.parse(html)
    val checkpointSuite = BasicCheckpointSuite.defaultSuite()
    val runner = new TestRunner(doc, checkpointSuite)
  }

  /**
   * (1) No title
   * (2) Missing img alt
   * (3) Bold tag
   * (5) No copyright
   */
  trait BadHtml {
    
    def html =
      """
        |<html>
        |<head>
        |<style>.theImage {width: 200px; height: 100px</style>
        |</head>
        |<body>
        |<h1>This Is The Title Of The Web Page</h1>
        |<p>Follow <i>this</i> <a href="www.site.com">link</a> to <b>learn more</b>.</p>
        |<p><img src="image.png" class="theImage" /></p>
        |</body>
        |</html>
      """.stripMargin
  }

  trait GoodHtml {
    def html =
      """
        |<html>
        |<head>
        |<title>A Title</title>
        |<link href="style.css" rel="stylesheet" />
        |</head>
        |<body>
        |<h1>Title</h1>
        |<other>Follow this <a href="www.site.com">link</a> to <strong>earn more</strong>.</other>
        |<div>Copyright &copy; 2014</div>
        |</body>
        |</html>
      """.stripMargin
  }

  behavior of "TestRunner"

  it should "run tests and not find errors" in new Fixture with GoodHtml {
    val results = runner.resultsOfTests()
    results.passes should have size(checkpointSuite.size)
    results.failures should have size (0)
    results.total should equal (checkpointSuite.size)
  }

  it should "run tests and find errors" in new Fixture with BadHtml {
    val results = runner.resultsOfTests()
    val expectedPasses = 3 // passes underline and heading test
    results.passes should have size(expectedPasses)
    results.failures should have size (checkpointSuite.size - expectedPasses)
    results.total should equal (checkpointSuite.size)
  }

  trait TestSuiteFixture {
    val checkpointSuite = BasicCheckpointSuite.defaultSuite()
    val fileName: String
    lazy val html = readFileContents(SamplePagesDirectory + fileName)
    lazy val doc = Jsoup.parse(html)
  }

  it should "run tests and find errors in a web page" in new TestSuiteFixture {

    val fileName = "page_with_errors.html"
    val runner = new TestRunner(doc, checkpointSuite)
    val results = runner.resultsOfTests()

    val expectedPasses = 2 // passes underline
    results.passes should have size(expectedPasses)
    results.failures should have size (checkpointSuite.size - expectedPasses)
    results.total should equal (checkpointSuite.size)
  }

  /*
  def highlightAndFile(doc: Document, fileName: String, failures:List[(Checkpoint,Result)]) {
    new PageHighlightReporter().create(doc, failures)
    writeToFile(SamplePagesDirectory + fileName + "_highlighted.html", doc.html())
  }

  def highlightSourceAndFile(doc: Document, fileName: String, failures:List[(Checkpoint,Result)]) {
    val html = new PageSourceHighlightReporter().create(doc, failures)
    writeToFile(SamplePagesDirectory + fileName + "_highlighted_source.html", html)
  }
  */

}
