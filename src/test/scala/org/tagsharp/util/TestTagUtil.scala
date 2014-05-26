package org.tagsharp.util

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.tagsharp.util.TagUtil.{tagOpenRegex, tagCloseRegex}
import org.tagsharp.util.TagUtil.{tagOpenEscapedRegex, tagCloseEscapedRegex}
import org.tagsharp.util.TagUtil.{replaceEscapedOpenTag, replaceEscapedCloseTag}


class TestTagUtil extends FlatSpec with ShouldMatchers with CustomMatchers {

  behavior of "TagUtil"

  it should "match open tags" in {

    val spanOpen = tagOpenRegex("span").r

    spanOpen should ( find ("<span>") inside ("<span>"))
    spanOpen should ( find ("<span>") inside ("<span>hello</span>"))
    spanOpen should ( find ("<span id=\"1234\">") inside ("<span id=\"1234\">hello</span>"))
    spanOpen should ( find ("<span id='1234' >") inside ("<span id='1234' >hello</span>"))
    spanOpen should ( find ("<span id='1234'>") inside ("<p><span id='1234'>hello</span> world</p>"))
    spanOpen should ( find ("<span \nid='1234'>") inside ("<p><span \nid='1234'>hello</span> world</p>"))
    spanOpen should not ( find ("< span>") inside ("< span>"))
    spanOpen should not ( find ("<spans>") inside ("<spans>"))

    tagOpenRegex("p").r should ( find ("<p id='1234'>") inside ("<div><p id='1234'>hello world</p></div>"))

  }

  it should "match close tags" in {

    val spanClose = tagCloseRegex("span").r

    spanClose should ( find ("</span>") inside ("<span>hello</span>"))
    spanClose should ( find ("</span >") inside ("<span>hello</span >"))
    spanClose should ( find ("</span\n>") inside ("<spans>hello</span\n>")) // forgiving
    spanClose should not ( find ("</spans>") inside ("<spans>hello</spans>"))
  }

  it should "match escaped open tags" in {

    val spanOpen = tagOpenEscapedRegex("span").r

    spanOpen should ( find ("&lt;span&gt;") inside ("&lt;span&gt;"))
    spanOpen should ( find ("&lt;span&gt;") inside ("&lt;span&gt;hello&lt;/span&gt;"))
    spanOpen should ( find ("&lt;span id=\"1234\"&gt;") inside ("&lt;span id=\"1234\"&gt;hello&lt;/span&gt;"))
    spanOpen should ( find ("&lt;span id='1234' &gt;") inside ("&lt;span id='1234' &gt;hello&lt;/span&gt;"))
    spanOpen should ( find ("&lt;span id='1234'&gt;") inside ("&lt;p&gt;&lt;span id='1234'&gt;hello&lt;/span&gt; world&lt;/p&gt;"))
    spanOpen should ( find ("&lt;span \nid='1234'&gt;") inside ("&lt;p&gt;&lt;span \nid='1234'&gt;hello&lt;/span&gt; world&lt;/p&gt;"))
    spanOpen should not ( find ("&lt; span&gt;") inside ("&lt; span&gt;"))
    spanOpen should not ( find ("&lt;spans&gt;") inside ("&lt;spans&gt;"))

  }

  it should "match escaped close tags" in {

    val spanClose = tagCloseEscapedRegex("span").r

    spanClose should ( find ("&lt;/span&gt;") inside ("&lt;span&gt;hello&lt;/span&gt;"))
    spanClose should ( find ("&lt;/span &gt;") inside ("&lt;span&gt;hello&lt;/span &gt;"))
    spanClose should ( find ("&lt;/span\n&gt;") inside ("&lt;spans&gt;hello&lt;/span\n&gt;")) // forgiving
    spanClose should not ( find ("&lt;/spans&gt;") inside ("&lt;spans&gt;hello&lt;/spans&gt;"))
  }

  it should "find and replace escaped open tag with unescaped" in {

    def replacing = replaceEscapedOpenTag("error-tag") _

    replacing("&lt;error-tag&gt;") should equal ("<error-tag>")
    replacing("&lt;error-tag &gt;") should equal ("<error-tag >")
    replacing("&lt;error-tag id='attr-value' &gt;") should equal ("<error-tag id='attr-value' >")
    replacing("&lt;error-tag id=\"attr-value\" &gt;") should equal ("<error-tag id=\"attr-value\" >")
    replacing("&lt;error-tag id=\"value1\" data-error=\"1234\" &gt;") should equal ("<error-tag id=\"value1\" data-error=\"1234\" >")
    replacing("&lt;error-tags&gt;") should not equal ("<error-tags>")
    replacing("&lt;/error-tag&gt;") should not equal ("</error-tag>")

    replacing("&lt;error-tag data-error=\"checkpointId\" style=\"padding: 1px 3px 1px 3px; background-color: #FFEE08 !important; border: solid 1px #FF1100 !important;\"&gt;"
    ) should equal("<error-tag data-error=\"checkpointId\" style=\"padding: 1px 3px 1px 3px; background-color: #FFEE08 !important; border: solid 1px #FF1100 !important;\">")

  }

  it should "find and replace escaped close tag with unescaped" in {

    def replacing = replaceEscapedCloseTag("span") _

    replacing("&lt;/span&gt;") should equal ("</span>")
    replacing("&lt;/span &gt;") should equal ("</span>")
    replacing("&lt;span&gt;") should not equal ("<span>")
    replacing("&lt;/spans&gt;") should not equal ("</spans>")
  }

}
