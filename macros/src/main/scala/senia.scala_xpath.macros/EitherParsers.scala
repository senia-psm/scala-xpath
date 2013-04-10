package senia.scala_xpath.macros

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers
import util.matching.Regex
import util.parsing.input.{Position, Reader}

trait EitherParsers extends Parsers {
  type R

  type Elem = Either[Char, R]

  def abort: Nothing

  class SeqReader(es: IndexedSeq[Either[Char, R]], init: Int = 0, line: Int = 1, column: Int = 1) extends Reader[Either[Char, R]]{
    self =>
    def first: Either[Char, R] = es(init)

    def rest: Reader[Either[Char, R]] =
      if (atEnd)
        this
      else
        new SeqReader(
          es,
          init + 1,
          line + (if (first == Left('\n')) 1 else 0),
          if (first == Left('\n')) 1 else column + 1
        )

    def pos: Position = new Position {
      val column: Int = self.column

      val line: Int = self.line

      val lineContents: String =
        es.
          drop(init).
          takeWhile { e => e.isLeft && e != Left('\n') }.
          map{
          case Left(ch) => ch
          case _ => abort
        }(collection.breakOut)
    }

    def atEnd: Boolean = init >= es.length
  }

  def stringToLeftSeq(s: String): List[Left[Char, R]] = augmentString(s).map{ Left(_) }(collection.breakOut)
  implicit def stringToParser(s: String): Parser[String] = accept(stringToLeftSeq(s)) ^^^ s
  implicit def charToParser(c: Char): Parser[Char] = Left(c) ^^^ c
  implicit def regexToParser(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      @annotation.tailrec def getString(in: Input, cs: List[Char] = Nil): String = {
        if (in.atEnd)
          cs.reverse.mkString
        else
          in.first match {
            case Right(_) => cs.reverse.mkString
            case Left(ch) => getString(in.rest, ch :: cs)
          }
      }

      val str = getString(in)

      r.findPrefixMatchOf(str) match {
        case Some(matched) => Success(matched.matched, in.drop(matched.end))
        case None => {
          val found = if (in.atEnd) "end of source" else s"`${in.first}'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in)
        }
      }
    }
  }

}

