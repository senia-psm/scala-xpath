package senia.scala_xpath.parser

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

import senia.scala_xpath.model._
import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers
import util.matching.Regex
import util.parsing.input.{Position, Reader}

class MixedXPathParsers[T]  extends Parsers {

  class SeqReader(es: IndexedSeq[Either[Char, T]], init: Int = 0, line: Int = 1, column: Int = 1) extends Reader[Either[Char, T]]{
    self =>
    def first: Either[Char, T] = es(init)

    def rest: Reader[Either[Char, T]] =
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
          map{ case Left(c) => c }(collection.breakOut)
    }

    def atEnd: Boolean = init >= es.length
  }

  type Elem = Either[Char, T]
  def stringToLeftSeq(s: String): List[Left[Char, T]] = augmentString(s).map{ Left(_) }(collection.breakOut)
  implicit def stringToParser(s: String): Parser[String] = accept(s)(stringToLeftSeq) ^^^ s
  implicit def charToParser(c: Char): Parser[Char] = Left(c) ^^^ c
  implicit def regexToParser(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      @annotation.tailrec def getString(in: Input, cs: List[Char] = Nil): String = {
        if (in.atEnd)
          cs.reverse.mkString
        else
          in.first match {
            case Right(_) => cs.reverse.mkString
            case Left(c) => getString(in.rest, c :: cs)
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

  def locationPath: Parser[LocationPath] = absoluteLocationPath | relativeLocationPath
  def slash: Parser[String] = ws ~> "/" <~ ws
  def dblSlash: Parser[String] = ws ~> "//" <~ ws
  def absoluteLocationPath: Parser[AbsoluteLocationPath] = abbreviatedAbsoluteLocationPath | slash ~> opt(relativeLocationPath) ^^ { AbsoluteLocationPathCommon }
  def abbreviatedAbsoluteLocationPath: Parser[AbbreviatedAbsoluteLocationPath] = dblSlash ~> relativeLocationPath ^^ { AbbreviatedAbsoluteLocationPath }
  def relativeLocationPath: Parser[RelativeLocationPath] = step ~ rep(stepSep ~ step ^^ { case sep ~ st => sep -> st }) ^^ { case s ~ ss => RelativeLocationPath(s, ss) }
  def stepSep: Parser[StepSep] = dblSlash ^^^ AbbreviatedStepSep | slash ^^^ StepSepCommon
  def step: Parser[Step] = abbreviatedStep | axisSpecifier ~ nodeTest ~ rep(predicate) ^^ { case as ~ nt ~ ps => StepCommon(as, nt, ps) }
  def abbreviatedStep: Parser[AbbreviatedStep] = `..` ^^^ ParentStep | `.` ^^^ SelfStep
  def axisSpecifier: Parser[AxisSpecifier] = axisName <~ `::` ^^ { AxisSpecifierCommon } | opt(`@`) ^^ { _.map{ _ => AttributeAxis}.getOrElse(ElementAxis) }
  def axisName: Parser[AxisName] = ws ~> "[a-z-]+".r <~ ws ^? (
    {
      case "ancestor" => Ancestor
      case "ancestor-or-self" => AncestorOrSelf
      case "attribute" => Attribute
      case "child" => Child
      case "descendant" => Descendant
      case "descendant-or-self" => DescendantOrSelf
      case "following" => Following
      case "following-sibling" => FollowingSibling
      case "namespace" => Namespace
      case "parent" => Parent
      case "preceding" => Preceding
      case "preceding-sibling" => PrecedingSibling
      case "self" => Self
    },
    n => s"""Axis name required, but "$n" found"""
    )
  def nodeTest: Parser[NodeTest] = "processing-instruction" ~ `(` ~> literal <~ `)` ^^ { InstructionNodeTest } | nodeTypeTest | nameTest
  def nodeTypeTest: Parser[NodeTypeTest] = nodeType <~ `(` ~ `)` ^^ { NodeTypeTest }
  def predicate: Parser[Predicate] = `[` ~> predicateExpr <~ `]` ^^ { Predicate }
  def predicateExpr: Parser[PredicateExpr] = expr ^^ { PredicateExpr }
  def expr: Parser[Expr] = orExpr ^^ { Expr }
  def primaryExpr: Parser[PrimaryExpr] = variableReference ^^ { VariableExpr } | `(` ~> expr <~ `)` ^^ { GroupedExpr } | functionCall ^^ { FunctionCallExpr } | number ^^ { NumberExpr } | literal ^^ { LiteralExpr }
  def functionCall: Parser[FunctionCall] = functionName ~ `(` ~ repsep(argument, `,`) <~ `)` ^^ { case fn ~ _ ~ as => FunctionCall(fn, as) }
  def argument: Parser[Argument] = expr ^^ { Argument }
  def unionExpr: Parser[UnionExpr] = rep1sep(pathExpr, ws ~> '|' <~ ws) ^^ { UnionExpr }
  def pathExpr: Parser[PathExpr] = locationPath ^^ { LocationPathExpr } ||| filterExpr ~ opt(( dblSlash | slash ) ~ relativeLocationPath) ^^ {
    case f ~ None => FilterPathExpr(f)
    case f ~ Some(s ~ rp) => FilterPathExpr(f, Some(rp), s == "//")
  }
  def filterExpr: Parser[FilterExpr] = primaryExpr ~ rep(predicate) ^^ { case pe ~ ps => FilterExpr(pe, ps) }
  def orExpr: Parser[OrExpr] = rep1sep(andExpr, ws ~> "or" <~ ws) ^^ { OrExpr }
  def andExpr: Parser[AndExpr] = rep1sep(equalityExpr, ws ~> "and" <~ ws) ^^ { AndExpr }
  def equalityExpr: Parser[EqualityExpr] = relationalExpr ~ rep( equality ~ relationalExpr ^^ { case e ~ r => e -> r } ) ^^ { case r ~ rs => EqualityExpr(r, rs) }
  def equality: Parser[Equality] = ws ~> ( '=' ^^^ Equal | "!=" ^^^ NotEqual ) <~ ws
  def relationalExpr: Parser[RelationalExpr] = additiveExpr ~ rep( relation ~ additiveExpr ^^ { case r ~ a => r -> a } ) ^^ { case ae ~ as => RelationalExpr(ae, as) }
  def relation: Parser[Relation] = ws ~> ( "<=" ^^^ LessOrEqual | '<' ^^^ Less | ">=" ^^^ GreaterOrEqual | '>' ^^^ Greater ) <~ ws
  def additiveExpr: Parser[AdditiveExpr] = multiplicativeExpr ~ rep( additiveOperation ~ multiplicativeExpr ^^ { case ao ~ me => ao -> me } ) ^^ { case me ~ ms => AdditiveExpr(me, ms) }
  def additiveOperation: Parser[AdditiveOperation] = ws ~> '+' <~ ws ^^^ Addition | ws ~> '-' <~ ws ^^^ Subtraction
  def multiplicativeExpr: Parser[MultiplicativeExpr] = unaryExpr ~ rep( multiplicativeOperator ~ unaryExpr ^^ { case o ~ e => o -> e } ) ^^ { case ue ~ us => MultiplicativeExpr(ue, us) }
  def multiplicativeOperator: Parser[MultiplicativeOperator] = multiplyOperator | ws ~> "div" <~ ws ^^^ Div | ws ~> "mod" <~ ws ^^^ Mod
  def multiplyOperator: Parser[MultiplyOperator.type] = ws ~> '*' <~ ws ^^^ MultiplyOperator
  def unaryExpr: Parser[UnaryExpr] = rep(ws ~> '-' <~ ws) ~ unionExpr ^^ { case ms ~ u => UnaryExpr(u, ms.map{ _ => Subtraction }) }
  def literal: Parser[Literal] = ws ~> ( """"[^"]*"""".r | """'[^']*'""".r ) <~ ws ^^ { Literal } //"
  def number: Parser[Number] = ws ~> ( digits ~ opt(`.` ~> opt(digits)) ^^ { case i ~ f => Number(Some(i), f.isDefined, f.flatten) } | `.` ~> digits ^^ { ds => Number(None, true, Some(ds) ) } ) <~ ws
  def digits: Parser[Digits] = """[0-9]+""".r ^^ { Digits }
  def functionName: Parser[FunctionName] = ws ~> qName <~ ws ^? (
    { case n if !NodeType.all.map{ _.toString }.apply(n.toString) => FunctionName(n) },
    { case n => s"""Function name expected, but node type "$n" found""" }
    )
  def variableReference: Parser[VariableReference] = ws ~ '$' ~> qName <~ ws ^^ { VariableReference }
  def nameTest: Parser[NameTest] = ws ~> ( multiplyOperator ^^^ NameTestAll | nCName <~ (':' ~ multiplyOperator) ^^ { NCNameTest } | qName ^^ { QNameTest } ) <~ ws
  def nodeType: Parser[NodeType] = ws ~> ( "comment" ^^^ Comment | "text" ^^^ Text | "processing-instruction" ^^^ ProcessingInstruction | "node" ^^^ Node ) <~ ws
  def exprWhitespace: Parser[ExprWhitespace] = rep1(accept("Whitespace", { case Left(c) if ExprWhitespace.allowedChars(c) => c })) ^^ { cs => ExprWhitespace(cs.mkString) }
  def ws: Parser[Unit] = opt(exprWhitespace) ^^^ { () }
  def qName: Parser[QName] = nCName ~ ':' ~ nCName ^^ { case p ~ _ ~ lp => QName(Some(p), lp) } | nCName ^^ { QName(None, _) }
  def nCName: Parser[NCName] = codePoint("NCName start code point", NCName.nameStartCodePoints) ~ rep(codePoint("NCName code point", NCName.nameCodePoints)) ^^ { case c ~ cs => NCName(c + cs.mkString) }
  def codePoint(expected: String, cs: Set[Int]): Parser[String] = new Parser[String] {
    def apply(in: Input): ParseResult[String] = {
      if (in.atEnd)
        Failure(f"""$expected%s expected but end found""", in)
      else if (in.first.isRight)
        Failure(f"""$expected%s expected but "${ in.first }" found""", in)
      else {
        val Left(first) = in.first
        val source = (Seq(first) ++ (if (in.rest.atEnd) None else in.rest.first.left.toOption)).mkString

        val codePoint = if (source.length == 1) first.toInt else source.codePointAt(0)

        if (!cs(codePoint))
          Failure(f"""$expected%s expected but "$codePoint%c"(0x$codePoint%h) found""", in)
        else
          Success(Character.toChars(codePoint).mkString, in.drop(Character.charCount(codePoint)))
      }
    }
  }

  //ExprToken:
  def `(`: Parser[ExprToken] = ws ~ '(' ~ ws ^^^ SimpleExprToken.`(`
  def `)`: Parser[ExprToken] = ws ~ ')' ~ ws ^^^ SimpleExprToken.`)`
  def `[`: Parser[ExprToken] = ws ~ '[' ~ ws ^^^ SimpleExprToken.`[`
  def `]`: Parser[ExprToken] = ws ~ ']' ~ ws ^^^ SimpleExprToken.`]`
  def `..`: Parser[ExprToken] = ws ~ ".." ~ ws ^^^ SimpleExprToken.`..`
  def `.`: Parser[ExprToken] = ws ~ '.' ~ ws ^^^ SimpleExprToken.`.`
  def `@`: Parser[ExprToken] = ws ~ '@' ~ ws ^^^ SimpleExprToken.`@`
  def `,`: Parser[ExprToken] = ws ~ ',' ~ ws ^^^ SimpleExprToken.`,`
  def `::`: Parser[ExprToken] = ws ~ "::" ~ ws ^^^ SimpleExprToken.`::`

  def parseAll[T2](p: Parser[T2], in: Reader[Elem]): ParseResult[T2] =
    phrase(p)(in)

  def apply(s: String) = parseAll(locationPath, new SeqReader(augmentString(s).map{ Left(_) }))
}
