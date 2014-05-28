About
=====

TagSharp is a Scala library to validate HTML documents parsed by Jsoup. It can be used to inspect the elements, attributes and text nodes for compliance within markup.

It is composed of a validation rule parser that constructs tests (using a custom DSL), a test runner to apply these tests to an HTML document and lastly a highlighting utility to visualise errors discovered within the document.


How Does it Work?
-----------------

TagSharp constructs tests by parsing rules from character strings and constructing expression evaluator functions. These tests (each of which contains an evaluator function to perform the work of testing) are applied in turn to a Jsoup HTML document using a test runner. If a test fails then a collection of errors are returned by the test runner indicating whereabouts in the document the problems occurred. An error highlighter can then use these error markers to figure out where to insert error highlighting markup.



Examples
--------

Here are a few example validation rules to show what can be done:


A test to assert that the web page contains an &lt;h1&gt; heading:

    <body> should contain <h1>

A test to assert that heading text does not contain more than X words:

    elements <h1>, <h2>, <h3>, <h4>, <h5>, <h6> should contain at most 6 words

A test to ensure the web page does not include underlined text (note: a fuller example would include a check with style attributes and stylesheets too):

    <body> should not contain <u>

A test to see if a page contains deprecated tags or attribute:

    elements <applet>, <object>, <*[bgcolor]> should not occur

A test to assert whether inline style attributes are being used when they shouldn't:

    <*[style]> should not occur

A test to ensure font sizes are expressed using relative not absolute units:

    <style>, [style] in selection "body *" should not match '''font-size:\s*[\d\.]+(px|pt|pc|in|cm|mm)'''


Error Visualisation
-------------------

Error visualisation can dramatically improve the usability of a validation tool. Highlighting can optionally be applied to the Jsoup Document after test(s) have been run. Applying highlighting actually changes the document structure so that is why it is done last.

Todo ...


Code
----

Todo ...


DSL Documentation
=================

A validation rule is formed of three parts:

    {selection clause} should {boolean predicate clause}

Firstly the *selection clause* determines what elements and/or attributes are to be tested. This is followed by the should word. Finally there is a boolean predicate clause that tests each item in the selection.


Selection Clauses
-----------------

Selection clauses determine which elements and/or attributes are to be tested:

| Selection Clause | Explanation | Example and Explanation |
| ---------------- | ---------- | ----------------------- |
| elements &lt;tag&gt;, &lt;tag&gt;, ... | selects one or more elements | elements &lt;h1&gt;, &lt;h2&gt; |
| &lt;tag&gt; | shorthand reference to single element | &lt;div&gt; |
| &lt;tag[attr]&gt; | selects a tag containing the given element. Note the subject of the selection is the element not the attribute. The assertion applies to the element. | &lt;img[src]&gt; |
| &lt;*&gt; | selects all elements, can also include an attribute sub-select | &lt;\*&gt; should ... <br/> &lt;\*[style]&gt; should not occur |
| attribute [attr] in &lt;tag&gt; | selects an attribute within an element. The attribute name is delineated with square parenthesis as is done in the CSS selector syntax. The subject of the selection is the attribute and the assertion applies to the attribute value. | [src] in &lt;img&gt; |
| [attr] in selection "..." | a variation on the above | |
| selection "..."  | a CSS selector expression in double quotes passed to Jsoup. This might be used for more elaborate selections containing regular expressions. | selection "head meta[http-equiv=content-language]" |


Boolean Predicate Clauses
-------------------------

Boolean predicates act on the selection to yield a result of either true or false. If the result is false then the test is considered failed.

| Assertion Clause        | Explanation | Example and Explanation |
| ----------------------- | ----------- | ----------------------- |
| occur | asserts that at least one item was found with the selection clause | &lt;h1&gt; should occur |
| occur n times | asserts that n items were returned by a selection clause | &lt;h1&gt; should not occur 2 times |
| occur {comparator} n times | checks for n occurrences within a selection against a comparator (less than, <, more than, >, at most, <=, at least, >=, exactly, =>) | &lt;p&gt; should occur less than 20 times <br/> &lt;h1&gt; should not occur more than once <br/> |
| contain {comparator} x words/characters | asserts that each item in a selection contains x number of words or characters | |
| contain children "h1, h2, h3" | asserts that items in a selection contain child elements. Note this ought to have been "contain elements &lt;h1&gt;, &lt;h2&gt;, &lt;h3&gt;." and will be updated in a future version. | |
| contain &lt;tag&gt; | asserts that items in a selection contain a particular element | &lt;head&gt; should contain &lt;title&gt; |
| contain selection "..." | asserts that items in a selection contain a sub-selection (with at least one item returned in the sub-selection) | |
| match {start/end/all} (?si) {regex} | asserts that selection text matches a regular expression. If the selection item is an element it matches the nested inner text. If an attribute it matches the attribute value. | &lt;title&gt; should match start '''Website\s.*''' |
| match words {exact case} ("regex1", "regex2", ...) | special case that allows matching on multiple regular expressions. Optionally this can assert that text matches are of the same capitalisation as the regular expression. An 'exact case' test is therefore two tests in one. | |
| not | The 'not' predicate is a special case. It can optionally precede another predicate to invert the result. | &lt;*[style]&gt; should not occur |


Other Syntax
------------

*Strings* are represented with double quotes:

    "hello world"

*Raw strings* containing regular expressions can be escaped with triple single quotes to avoid the need to escape characters within the regular expression itself:

    '''\.*'''
    (compare that with "\\.*", where backslash needs escaping)

A *comment* can prepend the expression and is ignored by the expression evaluator. It is delineated using slash and star format. Comments cannot be used anywhere except the beginning.

    /* This part of the expression is ignored */ <title> should occur at least once


Todo
----

A REPL would make this library easier to get up and running with.


About the Implementation
------------------------

The selector part of this DSL for the most part wraps the CSS selector functionality of Jsoup using some syntactic sugar to improve readability. The selector functionality within Jsoup has great power and flexibility so it seems sensible to delegate to it where possible.



Further Reading
---------------

Before working on this proejct I found reading these books extremely helpful. [Debasish Gosh](http://debasishg.blogspot.co.uk/) in particular is very passionate about the subject and will fill you with enthusiasm for the task ahead!

* *Debasish Gosh, DSLs in Action, Manning Publications Co., 2011* - explains in great detail the difference between internal and external DSLs providing many examples in Scala, Groovy and Clojure.
* *Odersky et al, Programming in Scala (2nd Edition), Artima Press, 2010* - see the chapter on how to use the Parser Combinator library.
