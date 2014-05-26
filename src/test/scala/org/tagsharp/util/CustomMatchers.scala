package org.tagsharp.util

import scala.util.matching.Regex
import org.scalatest.matchers.{MatchResult, Matcher}
import org.tagsharp.test.{Pass, Result}


trait CustomMatchers {

  /**
   * @see http://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers
   */
  class PassMatcher extends Matcher[Result] {
    def apply(left: Result) = {
      val msg = "Result was not a Pass >> " + left
      MatchResult(
        left match {
          case Pass() => true
          case _ => false
        },
        msg, msg, msg, msg)
    }
  }

  def pass = new PassMatcher


  class RegexMatcher(pair: (CharSequence, String)) extends Matcher[Regex] {
    def apply(left: Regex) = {

      val term = pair._1
      val result = pair._2
      val msg = s"Error matching [$left] against [$term]"

      MatchResult(left findFirstIn term match {
        case Some(value) => result == value
        case None => false
      }, msg, msg, msg, msg)

    }
  }

  class Insider(result: String) {
    def inside(input: CharSequence) = new RegexMatcher(input, result)
  }

  def find(result: String) = new Insider(result)

}
