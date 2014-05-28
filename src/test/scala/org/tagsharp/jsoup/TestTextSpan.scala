package org.tagsharp.jsoup

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito
import org.jsoup.nodes.TextNode


class TestTextSpan extends FlatSpec with ShouldMatchers {

  behavior of "TextSpan"

  // Same as TestPageErrorHighlighter
  trait TextSpanFixture {

    // n0  [0   4]
    // n1        [5 ... 10]
    // n2              [8 ... 12]
    // n3                    [10 ... 14]
    // n4                               [15 ... 20]
    // n5                                       [19 ... 23]
    // n6                                                   [25 ... 30]

    val tn = Mockito.mock(classOf[TextNode])
    val t0 = TextSpan(0, 4, tn)
    val t1 = TextSpan(5, 10, tn)
    val t2 = TextSpan(8, 12, tn)
    val t3 = TextSpan(10, 14, tn)
    val t4 = TextSpan(15, 20, tn)
    val t5 = TextSpan(19, 23, tn)
    val t6 = TextSpan(25, 30, tn)
  }

  it should "find overlaps" in new TextSpanFixture {
    t0 overlaps (t0) should equal (true)
    t1 overlaps (t2) should equal (true)
    t2 overlaps (t1) should equal (true)
    t2 overlaps (t3) should equal (true)
    t3 overlaps (t2) should equal (true)
    t4 overlaps (t5) should equal (true)
    t5 overlaps (t4) should equal (true)
    t6 overlaps (t6) should equal (true)
  }

  it should "not find overlaps" in new TextSpanFixture {
    t0 overlaps (t1) should equal (false)
    t1 overlaps (t3) should equal (false)
    t3 overlaps (t1) should equal (false)
    t6 overlaps (t5) should equal (false)
  }

}
