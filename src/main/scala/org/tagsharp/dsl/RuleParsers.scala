package org.tagsharp.dsl

import scala.language.postfixOps
import util.parsing.combinator.JavaTokenParsers
import org.tagsharp.dsl.RuleAST._


object RuleParsers {

  /**
   * realString is a workaround for weird 2.10 behaviour:
   * stringLiteral no longer strips the double quotes
   * @see http://www.scala-lang.org/node/1352
   */
  trait UtilityParsers extends JavaTokenParsers {
    
    def realString: Parser[String] = stringLiteral ^^ {s => s.substring(1, s.length-1)}
    
    def regExpress:Parser[String] = """'''.+?'''""".r ^^ { // Does not escape escape characters
      r => r.toString.replaceAll("'''", "")
    }
    def concatString:Parser[String] = "concat("~>repsep((realString | regExpress), ",")<~")" ^^ {
      case strings @ List(_*) => strings.mkString("")
    }

    def stringLike: Parser[String] = (realString | regExpress | concatString)

    def comment:Parser[String] = """(?s)/\*(.*?)\*/""".r ^^ {r => r.toString}
  }


  trait SelectorParsers extends JavaTokenParsers with UtilityParsers {

    def tagName:Parser[String] = """<([^\s,<>])+>?""".r ^^ {r => {
      val s = r.toString
      s.substring(1, s.length-1)
    }}

    private def attrWord: Parser[String] = opt("attributes?".r) ^^ {
      case None => "attribute"
      case Some(a) => "attribute"
    }

    def attribute: Parser[String] = attrWord~>(realString | "["~>ident<~"]") ^^ {a => a}
    def selection: Parser[String] = "selection"~>stringLike ^^ {s => s}

    def selectionSelect: Parser[Selection] = selection ^^ ElementSelection
    def element: Parser[Selection] = tagName ^^ ElementSelection

    def attrSelect: Parser[AttributeSelection] =
      attribute~"in"~(selection | tagName) ^^ {
      case a~"in"~e => AttributeSelection(e, a)
    }

    def selector: Parser[Selection] = (
      attrSelect | element | selectionSelect
    )

    private def selectionsWord: Parser[String] = opt("selections" | "elements") ^^^ "selections"

    def selections: Parser[Selections] = selectionsWord~>repsep(selector, ",") ^^ {
      case List(s1) => Selections(Seq(s1))
      case s => Selections(s)
    }

  }


  trait NumberBoundaryParsers extends JavaTokenParsers with UtilityParsers {

    def times: Parser[Int] = ("once" | wholeNumber<~("times" | "time")) ^^ {
      case "once" => 1
      case x => x.toInt
    }

    /**
     * Implementation specific that typically implements a collection
     * exposing a single value, i.e. a count of items in the collection.
     */
    def countable: Parser[Countable]

    def containOrder: Parser[Predicate] = (singleBound | doubleBound)

    def order: Parser[Order] =
      (
        "<=" | "at most" |
        "<" | "less than" | "at most" |
        ">=" | "at least" |
        ">" | "greater than" | "more than" |
        "=" | "exactly"
      ) ^^ {
        case ("<" | "less than" | "at most") => LT()
        case ("<=" | "at most") => LTE()
        case (">" | "greater than" | "more than") => GT()
        case (">=" | "at least") => GTE()
        case ("=" | "exactly") => EQ()
      }

    private def singleBound: Parser[Contain] = order~wholeNumber~countable ^^ {
      case c~x~ct => Contain(ct, c, x.toInt)
    }

    private def doubleBound: Parser[Predicate] =
      (
        "between"~>wholeNumber~"and"~wholeNumber |
        "from"~>wholeNumber~"to"~wholeNumber)~countable ^^
      {
        case x~"and"~y~c => Between(c, x.toInt, y.toInt)
        case x~"to"~y~c => Between(c, x.toInt, y.toInt)
      }

  }

  trait TextMatchParsers extends JavaTokenParsers with UtilityParsers {

    /**
     * Examples:
     * match '''pattern''' (defaults to any (?si))
     * match any '''pattern''' (defaults to (?si))
     * match any (?si) '''pattern'''
     * match all (?si) '''pattern'''
     * match start (?si) '''pattern'''
     * match end (?si) '''pattern'''
     */
    def matchRegex:Parser [MatchText] =
      ("match(es)?".r)~>matcherPosition~matcherFlags~stringLike ^^ {
        case "any"~f~r => MatchRegex(f + ".*?" + r + ".*?")
        case "all"~f~r => MatchRegex(f + r)
        case "start"~f~r => MatchRegex(f + "^" + r + ".*")
        case "end"~f~r => MatchRegex(f + ".*" + r + "$")
      }

    def matcherPosition:Parser[String] = opt("any" | "all" | "start" | "end") ^^ {
      case None => "any"
      case Some(s) => s
    }

    def matcherFlags:Parser[String] = opt("""\(\?[sim]+\)""".r) ^^ {
      case None => "(?si)"
      case Some(r) => r.toString
    }

    def exactCase:Parser[Boolean] = opt("exact-case") ^^ {
      case None => false
      case Some(s) => true
    }

    def matchWords:Parser[MatchWords] = "match"~>"words"~>exactCase~"("~repsep(stringLike, ",")<~")" ^^ {
      case ec~"("~patterns => MatchWords(patterns, ec)
    }

    def matchText = (matchRegex | matchWords)
  }

  class RuleParser
      extends SelectorParsers
      with NumberBoundaryParsers
      with TextMatchParsers
      with UtilityParsers {

    def containAttribute: Parser[CssContain] = attribute ^^ {ContainAttribute(_)}
    
    // In practice this is being used to select using attribute selectors only
    // probably because the ContainAttribute is not flexible enough.
    def containSelection: Parser[CssContain] = selection ^^ {ContainSelection(_)}

    def children: Parser[CssContain] = ("children"~>realString | tagName) ^^ {ContainSubSelection(_)}

    def characters: Parser[Characters] = "characters?".r ^^^ Characters()
    def words: Parser[Words] = "words?".r ^^^ Words()
    
    def not: Parser[Not] = "not"~>predicate ^^ { t => Not(t) }
    
    def occur: Parser[Occur] = "occur"~>opt(order~times) ^^ {
      case None => Occur(GT(), 0)
      case Some(c~y) => Occur(c, y)
    }

    def contain: Parser[Predicate] = "contain"~>(containOrder | countable) ^^ {
      case c: Countable => Contain(c, GT(), 0)
      case p: Predicate => p
    }

    def countable: Parser[Countable] = (
      children | containSelection | containAttribute | words | characters
    )

    def predicate: Parser[Predicate] = (
      not | occur | contain | matchText
    )

    def rule: Parser[Rule] = (comment?)~>selections~"should"~predicate ^^ {
      case s~"should"~t => new Rule(s, t)
    }

    def parseRule(source: String):ParseResult[Rule] = parseAll(rule, source)
  }

}
