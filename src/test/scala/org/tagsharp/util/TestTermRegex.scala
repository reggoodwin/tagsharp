package org.tagsharp.util

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.tagsharp.util.TermRegex.makeString


class TestTermRegex extends FlatSpec with ShouldMatchers with CustomMatchers {

  behavior of "TermRegex"

  it should "make a pattern from the input term" in {
    makeString("company name") should equal ("""(?si)\bcompany\s+name\b""")
  }

  it should "match using StringBuilder char sequence" in {
    val sb = new StringBuilder
    List("The ", "Company", " Name") foreach (sb.append(_))
    TermRegex("company") should (find ("Company") inside (sb))
  }

  it should "case insensitive match single words" in {
    TermRegex("the") should ( find ("The") inside "The Company Name")
    TermRegex("the") should ( find ("the") inside "the Company Name" )
    TermRegex("company") should ( find ("Company") inside "The Company Name" )
    TermRegex("company") should ( find ("company") inside "The company Name" )
    TermRegex("name") should ( find ("Name") inside "The Company Name" )
    TermRegex("name") should ( find ("name") inside "The Company name" )
  }

  it should "not match on word prefixes, middles or suffixes" in {
    TermRegex("company") should not (find ("Company") inside "CompanyClose")
    TermRegex("company") should not (find ("Company") inside "companyClose")
    TermRegex("company") should not (find ("Company") inside "companyclose")
    TermRegex("company") should not (find ("Company") inside "CloseCompany")
    TermRegex("company") should not (find ("Company") inside "closecompany")
    TermRegex("company") should not (find ("Company") inside "closecompany ")
    TermRegex("company") should not (find ("Company") inside " companyclose")
  }

  it should "match common punctuation boundaries" in {
    TermRegex("company") should ( find ("Company") inside "Company-Name")
    TermRegex("company") should ( find ("Company") inside "Company,Name")
    TermRegex("company") should ( find ("Company") inside "Company.Name")
    TermRegex("company") should ( find ("Company") inside "Company's Name")
    TermRegex("company") should ( find ("Company") inside "Company-Name")
    TermRegex("name") should ( find ("Name") inside "Company-Name")
    TermRegex("name") should ( find ("Name") inside "Company,Name")
    TermRegex("name") should ( find ("Name") inside "Company.Name")
  }

}
