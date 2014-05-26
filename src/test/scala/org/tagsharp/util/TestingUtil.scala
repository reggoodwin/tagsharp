package org.tagsharp.util

import java.io.File
import scala.io.Source
import org.tagsharp.test.{CheckpointSuite, Test}
import org.mockito.Mockito.{mock, when}
import org.tagsharp.checkpoint.{Highlightable, Checkpoint}


object TestingUtil {

  /**
   * The location of test HTML files
   */
  val SamplePagesDirectory = """.\src\test\resources\sample_pages\"""

  def writeToFile(filename:String, content:String) {

    def loanPrintWriter(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }

    loanPrintWriter(new File(filename))(p => {
      p.println(content)
    })
  }

  /**
   * Read contents as UTF-8 to prevent java.nio.charset.UnmappableCharacterException
   * @see http://stackoverflow.com/questions/1757272/how-to-resolve-java-nio-charset-unmappablecharacterexception-in-scala-2-8-0
   */
  def readFileContents(path:String):String = {
    val file = Source.fromFile(path)("UTF-8")
    val sb = new StringBuilder
    for (line <- file.getLines()) sb.append(line)
    sb.toString
  }

  def timedOperation(name: String)(op: String => Unit) {
    val start = System.currentTimeMillis()
    op(name)
    val end = System.currentTimeMillis()
    val duration = end-start
    println(s"$name took $duration millis")
  }

  def mockTest(name:String):Test = {
    val t = mock(classOf[Test])
    when(t.toString).thenReturn(name)
    t
  }

}
