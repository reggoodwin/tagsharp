package org.tagsharp.reporter

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.tagsharp.util.TestingUtil._
import org.tagsharp.test.TestRunner
import org.tagsharp.checkpoint.BasicCheckpointSuite
import org.jsoup.Jsoup


class TestTextErrorHighlighter extends FlatSpec with ShouldMatchers {

  behavior of "TextErrorHighlighter"

  trait BadHtml {
    
    // Errors: (1) no title, (2) missing <img> alt attribute, (3) <b> tag, (5) no copyright

    def html =
      """
        |<html>
        |<head>
        |<style>.theImage {width: 200px; height: 100px</style>
        |</head>
        |<body>
        |<h1>Title</h1>
        |<p>Follow <i>this</i> <a href="www.site.com">link</a> to <b>learn more</b>.</p>
        |<p><b>This</b> is an <img src="image.png" class="theImage" /> tag.</p>
        |</body>
        |</html>
      """.stripMargin
  }

  it should "report errors" in new BadHtml {
    val checkpointSuite = BasicCheckpointSuite.defaultSuite()
    val doc = Jsoup.parse(html)
    val runner = new TestRunner(doc, checkpointSuite)
    val results = runner.testResults
    val highlighter = new TextErrorHighlighter()
    val report = highlighter.highlight(results)
  }

}
