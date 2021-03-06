package senia.scala_xpath.parser

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

import org.scalatest.FunSpec
import scala.language.implicitConversions
import org.scalatest.matchers.{ShouldMatchers, MatchResult, Matcher}

class ParserSpec extends FunSpec with ShouldMatchers {
  val parser = new XPathParsers

  def beSuccessful[T](s: String) = new Matcher[parser.ParseResult[T]] {
    def apply(r: parser.ParseResult[T]) =
      MatchResult(
        r.successful,
        s"""Parse result of "$s" was not successful: $r""",
        s"""Parse result of "$s" was successful: $r"""
      )
  }

  describe("A Parser") {
    for (example <- XPathExamples.examples) {
      it( s"""should parse $example""") {
        parser(example) should beSuccessful(example)
      }
    }
  }
}



