package org.tagsharp.reporter

import org.tagsharp.checkpoint.Checkpoint
import org.tagsharp.test.{Pass, Fail, Results, Result}


class TextErrorReporter {

  def createReport(results: Results):String = {
    return collect(results.failures ::: results.passes)
  }

  /**
   * @see http://en.wikipedia.org/wiki/Tick_%28check_mark%29
   */
  def collect(items: List[(Checkpoint, Result)]):String = {
    val sb = new StringBuilder
    items.foreach(pair => {
      val checkpoint = pair._1
      val result = pair._2
      val message = result match {
        case Fail(errors) => {
          "x %s (%s)\n\n\t%s\n".format(checkpoint.name, errors.size, errors)
        }
        case Pass() => "\u2714 "  + checkpoint.name
      }
      sb.append(message).append("\n")
    })
    sb.toString
  }

}
