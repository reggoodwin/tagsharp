package org.tagsharp.dsl

import org.jsoup.Jsoup
import org.tagsharp.dsl.RuleAST.{Fail, Pass}


class TestRuleParserCheckpoints extends BaseRuleParserTest {


  class GoodMetaDescription extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>A Page Title</title>
        |  <meta name="description"
        |  content="A description meta tag that provides just enough" />
        |</head><body></body></html>
      """.stripMargin
    )
  }

  class BadMetaDescription extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>A Page Title</title>
        |  <meta name="description"
        |  content="A description meta tag that says far too much about the page" />
        |</head><body></body></html>
      """.stripMargin
    )
    val meta = first("meta")
  }

  class ListItemDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>Title</title></head><body>
        |<ul>
        |  <li id="ulLi0">lowercase u1</li>
        |  <li id="ulLi1"><strong>Uppercase u2.</strong></li>
        |</ul>
        |<ol>
        |  <li id="olLi0">Uppercase o1. </li>
        |  <li id="olLi1">   lowercase o2</li>
        |</ol>
        |</body></html>
      """.stripMargin)
    val ulLi0 = byId("ulLi0")
    val ulLi1 = byId("ulLi1")
    val olLi0 = byId("olLi0")
    val olLi1 = byId("olLi1")
  }

  class BadStylesDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head>
        |<style type="text/css" id="style0">
        |  font-family: not allowed;
        |  font-size: 1.5px;
        |</style>
        |<style type="text/css" id="style1">
        |  /* style with no font family or font size */
        |  margin: 1em;
        |</style>
        |<body>
        |<div id="div1" style="font-family: not allowed;
        | font-size:
        |   1.5px;">
        |    Content here ...
        |</div>
        |</body>
        |</html>
      """.stripMargin)
    val div1 = byId("div1")
    val style0 = byId("style0")
  }

  class FramesetDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head>
        |<frameset id="frameset0">
        |  <frame id="frame0" src="webpage">...</frame>
        |</frameset>
        |</html>
      """.stripMargin)
    val frameset0 = byId("frameset0")
    val frame0 = byId("frame0")
  }

  class LongHeadingsDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head><body>
        |<h1>Rather Long Heading</h1>
        |</body></html>
      """.stripMargin)
    val h1 = first("h1")
  }

  class Doc5 extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |  <title>Page Title</title>
        |</head>
        |<body>
        |<h2>Page With No H1 Headings</h2>
        |<table class="red">
        |  <tr id="tr1"><th>heading</th></tr>
        |  <tr id="tr2"><td>cell</td></tr>
        |</table>
        |</body>
        |</html>
      """.stripMargin
    )
    val table = first("table")
    val tr0 = first("tr:eq(0)")
    val tr1 = first("tr:eq(1)")
    val th = first("th")
    val td = first("td")
  }

  class Doc6 extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |    <title>Page Title</title>
        |</head>
        |<body>
        |<h2>Page With No H1 Headings</h2>
        |<table>
        |  <tr><td bgcolor="red">fake heading row</td></tr>
        |  <tr><td>cell</td></tr>
        |</table>
        |</body>
        |</html>
      """.stripMargin
    )

    val table = first("table")
    val tr0 = first("tr:eq(0)")
    val tr1 = first("tr:eq(1)")
    val td0 = first("tr:eq(0) > td")
    val td1 = first("tr:eq(1) > td")
  }

  // A web page should contain an <h1> heading.

  withTerms("""<body> should contain <h1>""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass()) }
    new BadDoc { eval(terms) should equal (Fail(Nil)) }
  }

  // A web page should contain at least one heading.

  withTerms("""<body> should contain children "h1, h2, h3, h4, h5, h6" """) { terms =>
    new Doc3 { eval(terms) should equal (Pass()) }
    new BadDoc { eval(terms) should equal (Fail(Nil)) }
  }

  // Heading text should not contain more than X words.

  withTerms("""elements <h1>, <h2>, <h3>, <h4>, <h5>, <h6> should contain at most 4 words""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass()) }
    new Doc3 { eval(terms) should equal (Fail(Seq(h2))) }
  }

  // Text should not be underlined with the <u> element.

  withTerms("""<body> should not contain <u>""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass()) }
    new Doc4 { eval(terms) should equal (Fail(Seq(u))) }
  }

  // Tables should contain column headings.

  withTerms("""<table> should contain <th>""") { terms =>
    new Doc5 { eval(terms) should equal (Pass()) }
    new Doc6 { eval(terms) should equal (Fail(Nil)) }
  }

  // Do not add colour to table backgrounds.

  withTerms("""elements <table>, <tr>, <th>, <td> should not contain [bgcolor] """) { terms =>
    new Doc5 { eval(terms) should equal (Pass()) }
    new Doc6 { eval(terms) should equal (Fail(Seq(td0))) }
  }

  // Page titles should match a particular naming convention.

  withTerms("""<title> should match start '''Company\s+(.*+)''' """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass()) }
    new BadDoc { eval(terms) should equal (Fail(Seq(title))) }
  }

  // The length of the <title> element should be limited to X characters.

  withTerms("""<title> should contain <= 65 characters""") { terms =>

    new Doc3 { eval(terms) should equal (Pass()) }

    new Eval {
      val doc = Jsoup.parse(
        """
          |<html lang="en-us"><head>
          |  <title>Company | An extremely long page title that goes into too much detail</title>
          |</head><body></body></html>
        """.stripMargin
      )
      val title = first("title")
      eval(terms) should equal (Fail(Seq(title)))
    }
  }

  // It is an error for a web page to contain multiple <title> elements.

  withTerms("""<title> should not occur more than once""") { terms =>

    new GoodDoc { eval(terms) should equal (Pass()) }

    new Eval { // no title at all
      val doc = Jsoup.parse("""<html><head></head><body>Content ...</body></html>""")
      eval(terms) should equal (Pass())
    }

    new Eval {
      val doc = Jsoup.parse(
          """
          |<html><head>
          |<title id="t0">A Page Title</title>
          |<title id="t1">A Second Page Title</title>
          |</head>
          |<body></body></html>
          """.stripMargin
      )
      val t0 = byId("t0")
      val t1 = byId("t1")

      eval(terms) should equal (Fail(Seq(t0, t1)))
    }

  }

  // A web page should contain a metadata description.

  withTerms("""[content] in selection "head meta[name=description]" should occur""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass()) }
    new BadDoc { eval(terms) should equal (Fail(Seq(Attribute("content",meta0)))) }
  }

  // A web page metadata description should not be longer than X characters.

  withTerms("""[content] in selection "head meta[name=description][content]" should not contain more than 50 characters""") { terms =>
    new GoodMetaDescription { eval(terms) should equal (Pass()) }
    new BadMetaDescription { eval(terms) should equal (Fail(Seq(Attribute("content", meta)))) }
  }

  // A web page should not include a <meta> element requesting a refresh.

  withTerms("""selection "head meta[http-equiv=refresh]" should not occur""") { terms =>
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="content-type" content="text/html" /></head></html>""")
      eval(terms) should equal (Pass())
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="refresh" content="1" /></head></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(meta)))
    }
  }

  // A web page refresh should not include a time limit.

  withTerms("""selection "head meta[http-equiv=refresh][content~=0[1-9]+|[0-9]+]" should not occur""") { terms =>
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="refresh" content="nonsense" /></head></html>""")
      eval(terms) should equal (Pass())
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="refresh" content="01" /></head></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(meta)))
    }
  }

  // A web page should specify the natural language of its content.
  // There are 3 variations of this test that should be rolled into one.
  // @see http://www.w3schools.com/jsref/prop_meta_httpequiv.asp

  withTerms("""<html> should contain selection "[lang~=(?i)[a-z]{1,8}]" """) { terms =>
    new Eval {
      val doc = Jsoup.parse(
        """<html lang="EN-US"><head><title>Title</title></head><body /></html>"""
      )
      eval(terms) should equal (Pass())
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><title>A Page Title</title></head></html>""")
      eval(terms) should equal (Fail(Seq(html)))
    }
    new Eval {
      val doc = Jsoup.parse("""<html lang=" "><head><title>A Page Title</title></head></html>""")
      eval(terms) should equal (Fail(Seq(html)))
    }
  }
  withTerms("""<html> should contain selection "[xml:lang~=(?i)[a-z]{1,8}]" """) { terms =>
    new Eval {
      val doc = Jsoup.parse(
        """<html xml:lang="EN-US"><head><title>Title</title></head><body/></html>"""
      )
      eval(terms) should equal (Pass())
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><title>Title</title></head></html>""")
      eval(terms) should equal (Fail(Seq(html)))
    }
    new Eval {
      val doc = Jsoup.parse("""<html xml:lang=""><head><title>Title</title></head></html>""")
      eval(terms) should equal (Fail(Seq(html)))
    }
  }
  withTerms("""[content] in selection "head meta[http-equiv=content-language]" should match all "[a-z-]{1,8}" """) { terms =>

    new Eval {
      val doc = Jsoup.parse(
        """<html><head><meta http-equiv="content-language" content="en-US"></head><body/></html>"""
      )
      eval(terms) should equal (Pass())
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="content-language"></head><body/></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(Attribute("content",meta))))
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="content-language" content=""></head><body/></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(Attribute("content",meta))))
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="content-language" content="123"></head><body/></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(Attribute("content",meta))))
    }
    new Eval {
      val doc = Jsoup.parse("""<html><head><meta http-equiv="content-language" content="morethaneight"></head><body/></html>""")
      val meta = first("meta")
      eval(terms) should equal (Fail(Seq(Attribute("content",meta))))
    }
  }

  // A page should include a copyright symbol © and statement within the footer.

  class CopyrightDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head>
        |</head>
        |<body>
        |<!-- Jsoup converts &copy; to © -->
        |<div id="footer">
        |  &copy; 2014 Statement
        |</div>
        |</body>
        |</html>
      """.stripMargin)
  }

  withTerms("""<div[id=footer]> should match "© 2014 Statement" """) { terms =>
    new CopyrightDoc { eval(terms) should equal (Pass()) }
    new BadDoc { eval(terms) should equal (Fail(Seq(divFooter))) }
  }

  // A web page should include a link to a sitemap page.

  withTerms("""<a[href~=(.*?)(sitemap)(.*?)]> should occur at least once """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new BadDoc { eval(terms) should equal (Fail(Nil)) }
  }

  // Bullet points should be capitalised.

  withTerms("""<li> should match start (?s) '''\p{Lu}''' """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new ListItemDoc { eval(terms) should equal (Fail(Seq(ulLi0, olLi1))) }
  }

  // Bullet points should not finish with a full stop.

  withTerms("""<li> should not match end (?s) '''\.''' """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new ListItemDoc { eval(terms) should equal (Fail(Seq(ulLi1, olLi0))) }
  }

  // Headings should not include full stops.

  class FullStopHDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>Title</title></head>
        |<body>
        |<h1>Heading with Full Stop.</h1>
        |</body>
        |</html>
      """.stripMargin)
    val h1 = first("h1")
  }

  withTerms("""elements <h1>, <h2>, <h3>, <h4>, <h5>, <h6> should not match end (?s) '''\.''' """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new FullStopHDoc { eval(terms) should equal (Fail(Seq(h1))) }
  }

  class DeprecatedDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title></head>
        |<body bgcolor="so wrong">
        |<div id="div0">
        |    <applet> ... </applet>
        |</div>
        |<div id="div1" style="font-family: blubs; font-size: 1px;">
        |    <object> ... </object>
        |</div>
        |</body>
        |</html>
      """.stripMargin)
    val applet = first("applet")
    val obj = first("object")
    val div1 = byId("div1")
  }

  // Avoid using deprecated tags or attribute.

  withTerms("""elements <applet>, <object>, <*[bgcolor]> should not occur""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new DeprecatedDoc { eval(terms) should equal (Fail(Seq(applet, obj, body))) }
  }

  // Styles should not be used inline within a page.

  withTerms("""<*[style]> should not occur""") { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new DeprecatedDoc { eval(terms) should equal (Fail(Seq(div1))) }
  }

  // Font measurements should be relative not absolute.
  // This works but cannot be mixed with elements <style>, <*[style]> because
  // the check for [style] attribute is actually made mistakenly on the element text
  // """<style> should not match '''font-size:\s*[\d\.]+(px|pt|pc|in|cm|mm)''' """
  // todo: check <style>

  withTerms("""<style>, [style] in selection "body *" should not match '''font-size:\s*[\d\.]+(px|pt|pc|in|cm|mm)''' """) { terms =>
    new GoodDoc { eval(terms) should equal (Pass())}
    new BadStylesDoc { eval(terms) should equal (Fail(Seq(style0,  Attribute("style", div1)))) }
  }

  class StylesheetDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head>
        |<link href="main.css" />
        |<style>
        | @import url "main.css";
        |</style>
        |</head><body></body></html>
      """.stripMargin)
  }

  // Text should not be justified.
  // todo: merge the two rules

  class JustifyDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>Title</title></head><body>
        |  <style>/* no justify */</style>
        |  <style id="style0">
        |  div {
        |    text-align: justify;
        |  }
        |  </style>
        |  <p align="justify" id="p0">
        |    There is no justification for a para like this.
        |  </p>
        |</body></html>
      """.stripMargin)
    val style0 = byId("style0")
    val p0 = byId("p0")
  }

  withTerms("""/* UNFINISHED */ <style>, [style] in <*[style]> should not match '''text-align\s*:\s*justify''' """) { terms =>
    new EmptyDoc { eval(terms) should equal (Pass()) }
    new GoodDoc { eval(terms) should equal (Pass()) }
    new JustifyDoc { eval(terms) should equal (Fail(Seq(style0))) }
  }
  withTerms("""/* UNFINISHED */ [align] in <*[align]> should not match "justify" """) { terms =>
    new JustifyDoc { eval(terms) should equal (Fail(Seq(Attribute("align", p0)))) }
  }

  // The page should not include blinking text.
  // todo: should also be checking for style "text-decoration: blink"

  class BlinkDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html><head><title>Title</title></head><body>
        |  <style>/* no blink */</style>
        |  <style id="style0">
        |  h1 {
        |    text-decoration: blink;
        |  }
        |  </style>
        |  <p>A <blink>blinking paragraph</blink> as well.</p>
        |</body></html>
      """.stripMargin)
    val blink = first("blink")
    val style0 = byId("style0")
  }

  withTerms("""/* UNFINISHED */ <body> should not contain <blink>""") { terms =>
    new EmptyDoc { eval(terms) should equal (Pass()) }
    new GoodDoc { eval(terms) should equal (Pass()) }
    new BlinkDoc { eval(terms) should equal (Fail(/* style0, */ Seq(blink))) }
  }

  // A web page should include a Google Analytics tracking tag.
  // Problems extracting data nodes in selectors:
  // http://stackoverflow.com/questions/16780517/java-obtain-text-within-script-tag-using-jsoup
  // todo: should handle scripts with no tracking tag

  class TrackingDoc extends Eval {
    val doc = Jsoup.parse(
      """
        |<html>
        |<head><title>Title</title</head>
        |<body>
        | &lt;script type="text/javascript">
        |  function notATrackingScript() { /* ... */ }
        |</script>
        |<script type="text/javascript">
        |    var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
        |    /// ...
        |</script>
        |</body></html>
      """.stripMargin)
  }

  withTerms("""/* UNFINISHED */ <script> should match "gaJsHost" """) { terms =>
    new EmptyDoc { eval(terms) should equal (Pass()) }
    new TrackingDoc { eval(terms) should equal (Pass()) }
  }

}
