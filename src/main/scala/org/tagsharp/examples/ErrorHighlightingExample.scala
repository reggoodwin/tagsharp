package org.tagsharp.examples

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.tagsharp.checkpoint.{Checkpoint, ValidationTest}
import org.tagsharp.test.{CheckpointSuite, TestRunner, Results, Result, Pass, Fail}
import org.tagsharp.reporter.PageErrorHighlighter


/**
 * Demonstrates how to use a TestRunner to run a CheckpointSuite of tests 
 * over an HTML page parsed by Jsoup and annotate elements/attributes/text nodes
 * containing validation errors in such a way that they show up as red highlighting 
 * errors when the document is viewed in a browser.
 */
object ErrorHighlightingExample extends App {


  //
  // A sample page containing markup errors:
  //
  // 1. Element error: The page includes <u> element (yes, a bit contrived!)
  // 2. Attribute error: The <img> element lacks an 'alt' attribute
  // 3. Text node error: a word/phrase capitalisation error
  //

  val html = """
    <!doctype html>
    <html>
      <head>
        <title>Page Title</title>
      </head>
      <body>
        <p>The web page should not contain <u>underlined text</u> yet it does.</p>
        <p>Image elements like <img src="image.png" /> should contain an alt attribute.</p>
        <p>The organisation 'Smart organisation' has the wrong capitalisation.</p>
      </body>
    </html>
    """

  // Parse the HTML with Jsoup

  val doc: Document = Jsoup.parse(html)
  
  // Construct a suite of checkpoint tests

  val suite = new CheckpointSuite(List(
    Checkpoint(
      name = "A web page should not contain underlined text",
      test = new ValidationTest("<body> should not contain <u>"),
      id = "mock-id-1"
    ),
    Checkpoint(
      name = "Image elements should contain an 'alt' attribute",
      test = new ValidationTest("[alt] in <img> should occur"),
      id = "mock-id-2"
    ),
    Checkpoint(
      name = "The organisation name should be spelt with the correct casing",
      test = new ValidationTest("""<body> should match words exact-case ("Smart Organisation") """),
      id = "mock-id-3"
    )
  ))

  val testRunner = new TestRunner(doc, suite)

  // Run the test suite and get the results

  val results: Results = testRunner.testResults

  // Apply highlighting to the Jsoup document to show the errors

  val highlighter = new PageErrorHighlighter
  highlighter.highlight(doc, results.failures)

  // Print out HTML with error highlighting

  println(doc.html())

}