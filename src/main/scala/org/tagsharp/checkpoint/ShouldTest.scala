package org.tagsharp.checkpoint


import org.jsoup.nodes.Element
import org.tagsharp.dsl.RuleParsers.RuleParser
import org.tagsharp.dsl.RuleAST.MatchResult
import org.tagsharp.dsl.Attribute
import org.tagsharp.jsoup.TextSpan
import org.tagsharp.test.{ElementError, TextError, Test}
import org.tagsharp.test.{Fail, Pass, Result}


/**
 * A Test whose terms are expressed in the '{selector} should {predicate}'
 * format and pre-compiled by the RuleParser.
 */
class ShouldTest(terms: String) extends Test {

  val parser = new RuleParser()
  val parseResult = parser.parseRule(terms)
  require(!parseResult.isEmpty, "Rule did not parse [" + parseResult + "]")
  val rule = parseResult.get

  override def test(element: Element): Result = {
    val mr:MatchResult = rule.eval(element)

    mr match {
      case org.tagsharp.dsl.RuleAST.Pass() => Pass()
      case org.tagsharp.dsl.RuleAST.Fail(failExamples) => {
        val errors = failExamples.map({
          case e: Element => ElementError(e)
          case a: Attribute => ElementError(a.element)
          case t: TextSpan => TextError(t)
        }).toList
        Fail(errors)
      }
    }
  }

  override def toString = getClass.getSimpleName + "(" + terms + ")"
}
