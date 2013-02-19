package senia.scala_xpath.parser

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */
import org.scalatest.FunSpec
import scala.language.implicitConversions
import org.scalatest.matchers.{ShouldMatchers, MatchResult, Matcher}

class MixedParserSpec extends FunSpec with ShouldMatchers {
  val mixedParser = new MixedXPathParsers[Nothing]

  def beSuccessful2[T](s: String) = new Matcher[mixedParser.ParseResult[T]] {
    def apply(r: mixedParser.ParseResult[T]) =
      MatchResult(
        r.successful,
        s"""Parse result of "$s" was not successful: $r""",
        s"""Parse result of "$s" was successful: $r"""
      )
  }

  describe("A Mixed Parser") {
    for (example <- XPathExamples.examples) {
      it( s"""should parse $example""") {
        mixedParser(example) should beSuccessful2(example)
      }
    }
  }
}
