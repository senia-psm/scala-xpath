package senia.scala_xpath.macros

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

import scala.language.experimental.macros
import reflect.macros.Context
import senia.scala_xpath.model._
import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers
import util.matching.Regex
import util.parsing.input.{Position, Reader}
import scala.tools.reflect.StdContextTags

trait XPathParsers extends EitherParsers {
  val xc: Context
  import xc.universe.reify

  type R = xc.Expr[Any]

  type EParser[T] = Parser[xc.Expr[T]]

  def abort = xc.abort(xc.enclosingPosition, "should never get here")


  val lts = new { val lc: xc.type = xc } with Literals
  import lts._

  var varN = 0
  def nextVarName: String = {
    varN += 1
    "var" + varN
  }

  def locationPath: EParser[LocationPath] = absoluteLocationPath | relativeLocationPath
  def slash: Parser[String] = ws ~> "/" <~ ws
  def dblSlash: Parser[String] = ws ~> "//" <~ ws
  def absoluteLocationPath: EParser[AbsoluteLocationPath] =
    abbreviatedAbsoluteLocationPath |
      slash ~> opt(relativeLocationPath) ^^ {
        _.fold{ reify{ AbsoluteLocationPathCommon(None) } }{ lp => reify{AbsoluteLocationPathCommon(Some(lp.splice))} }
      }
  def abbreviatedAbsoluteLocationPath: EParser[AbbreviatedAbsoluteLocationPath] = dblSlash ~> relativeLocationPath ^^ {
    lp => reify{ AbbreviatedAbsoluteLocationPath(lp.splice) }
  }
  def relativeLocationPath: EParser[RelativeLocationPath] =
    step ~ rep(stepSep ~ step ^^ { case sep ~ st => sep -> st }) ^^ {
      case s ~ ss => reify{RelativeLocationPath(s.splice, lts.seqPairToExprSeq(ss).splice) }
    }
  def stepSep: Parser[StepSep] = dblSlash ^^^ AbbreviatedStepSep | slash ^^^ StepSepCommon
  def step: EParser[Step] =
    abbreviatedStep ^^ { s => lts.literal(s) } |
      axisSpecifier ~ nodeTest ~ rep(predicate) ^^ { case axs ~ nt ~ ps => reify{StepCommon(lts.literal(axs).splice, lts.literal(nt).splice, lts.seqExprToExprSeq(ps).splice)} }
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
  def predicate: EParser[Predicate] = `[` ~> predicateExpr <~ `]` ^^ { p => reify{Predicate(p.splice)} }
  def predicateExpr: EParser[PredicateExpr] = expr ^^ { e => reify{PredicateExpr(e.splice)} }
  def expr: EParser[Expr] = orExpr ^^ { e => reify{ Expr(e.splice) } }

  val stdContextTags = new { val tc: xc.type = xc } with StdContextTags
  import stdContextTags._
  def confirmType(e: xc.Expr[_], t: xc.Type): Boolean = (e.actualType weak_<:< t) || (xc.inferImplicitView(e.tree, e.actualType, t) != xc.universe.EmptyTree)

  def primaryExpr: EParser[PrimaryExpr] =
    accept("xc.Expr[Any]", { case Right(e) => e } ) ^? ({
        case e: xc.Expr[BigInt] if confirmType(e, tagOfBigInt.tpe) => reify{ CustomIntVariableExpr(VariableReference(QName(None, NCName(xc.literal(nextVarName).splice))), e.splice) }
        case e: xc.Expr[String] if confirmType(e, xc.universe.definitions.StringClass.toType) => reify{ CustomStringVariableExpr(VariableReference(QName(None, NCName(xc.literal(nextVarName).splice))), e.splice) }
      },
      e => s"Int, Long, BigInt or String expression expected, $e found."
      ) |
    variableReference ^^ { case VariableReference(n) => reify{ VariableExpr(VariableReference(lts.literal(n).splice)) } } |
    `(` ~> expr <~ `)` ^^ { r => reify{GroupedExpr(r.splice)} } |
    functionCall ^^ { f => reify{ FunctionCallExpr(f.splice) } } |
    number ^^ { case Number(i, wd, f) => reify{ NumberExpr(Number(lts.literal(i).splice, xc.literal(wd).splice, lts.literal(f).splice)) } } |
    literal ^^ { case l => reify{LiteralExpr(lts.literal(l).splice)} }
  def functionCall: EParser[FunctionCall] = functionName ~ `(` ~ repsep(argument, `,`) <~ `)` ^^ {
    case fn ~ _ ~ ags => reify{ FunctionCall(lts.literal(fn).splice, lts.seqExprToExprSeq(ags).splice) }
  }
  def argument: EParser[Argument] = expr ^^ { e => reify{ Argument(e.splice) } }
  def unionExpr: EParser[UnionExpr] = rep1sep(pathExpr, ws ~> '|' <~ ws) ^^ { es => reify{UnionExpr(lts.seqExprToExprSeq(es).splice)} }
  def pathExpr: EParser[PathExpr] =
    locationPath ^^ { lp => reify{LocationPathExpr(lp.splice)} } |||
      filterExpr ~ opt(( dblSlash | slash ) ~ relativeLocationPath) ^^ {
        case f ~ None => reify{FilterPathExpr(f.splice)}
        case f ~ Some(s ~ rp) => reify{FilterPathExpr(f.splice, Some(rp.splice), xc.literal(s).splice == "//")}
      }
  def filterExpr: EParser[FilterExpr] = primaryExpr ~ rep(predicate) ^^ { case pe ~ ps => reify{FilterExpr(pe.splice, lts.seqExprToExprSeq(ps).splice)} }
  def orExpr: EParser[OrExpr] = rep1sep(andExpr, ws ~> "or" <~ ws) ^^ { es => reify{OrExpr(lts.seqExprToExprSeq(es).splice)} }
  def andExpr: EParser[AndExpr] = rep1sep(equalityExpr, ws ~> "and" <~ ws) ^^ { es => reify{AndExpr(lts.seqExprToExprSeq(es).splice)} }
  def equalityExpr: EParser[EqualityExpr] = relationalExpr ~ rep( equality ~ relationalExpr ^^ { case e ~ r => e -> r } ) ^^ { case r ~ rs => reify{EqualityExpr(r.splice, lts.seqPairToExprSeq(rs).splice)} }
  def equality: Parser[Equality] = ws ~> ( '=' ^^^ Equal | "!=" ^^^ NotEqual ) <~ ws
  def relationalExpr: EParser[RelationalExpr] = additiveExpr ~ rep( relation ~ additiveExpr ^^ { case r ~ a => r -> a } ) ^^ { case ae ~ aes => reify{RelationalExpr(ae.splice, lts.seqPairToExprSeq(aes).splice)} }
  def relation: Parser[Relation] = ws ~> ( "<=" ^^^ LessOrEqual | '<' ^^^ Less | ">=" ^^^ GreaterOrEqual | '>' ^^^ Greater ) <~ ws
  def additiveExpr: EParser[AdditiveExpr] = multiplicativeExpr ~ rep( additiveOperation ~ multiplicativeExpr ^^ { case ao ~ me => ao -> me } ) ^^ {
    case me ~ ms => reify{AdditiveExpr(me.splice, lts.seqPairToExprSeq(ms).splice)}
  }
  def additiveOperation: Parser[AdditiveOperation] = ws ~> '+' <~ ws ^^^ Addition | ws ~> '-' <~ ws ^^^ Subtraction


  def multiplicativeExpr: EParser[MultiplicativeExpr] = unaryExpr ~ rep( multiplicativeOperator ~ unaryExpr ^^ {
    case o ~ e => o -> e } ) ^^ { case ue ~ us => reify{MultiplicativeExpr(ue.splice, lts.seqPairToExprSeq(us).splice)}
  }
  def multiplicativeOperator: Parser[MultiplicativeOperator] = multiplyOperator | ws ~> "div" <~ ws ^^^ Div | ws ~> "mod" <~ ws ^^^ Mod
  def multiplyOperator: Parser[MultiplyOperator.type] = ws ~> '*' <~ ws ^^^ MultiplyOperator
  def unaryExpr: EParser[UnaryExpr] = rep(ws ~> '-' <~ ws) ~ unionExpr ^^ { case ms ~ u => reify{UnaryExpr(u.splice, lts.seqExprToExprSeq(ms.map{ _ => reify{ Subtraction }}).splice )}}
  def literal: Parser[Literal] = ws ~> ( """"[^"]*"""".r | """'[^']*'""".r ) <~ ws ^^ { Literal } //"
  def number: Parser[Number] = ws ~> ( digits ~ opt(`.` ~> opt(digits)) ^^ { case i ~ f => Number(Some(i), f.isDefined, f.flatten) } | `.` ~> digits ^^ { ds => Number(None, withDot = true, Some(ds) ) } ) <~ ws
  def digits: Parser[Digits] = """[0-9]+""".r ^^ { Digits }
  def functionName: Parser[FunctionName] = ws ~> qName <~ ws ^? (
    { case n if !NodeType.all.map{ _.toString }.apply(n.toString) => FunctionName(n) },
    { case n => s"""Function name expected, but node type "$n" found""" }
    )
  def variableReference: Parser[VariableReference] = ws ~ '$' ~> qName <~ ws ^^ { VariableReference }
  def nameTest: Parser[NameTest] = ws ~> (
    multiplyOperator ^^^ NameTestAll |
      nCName <~ (':' ~ multiplyOperator) ^^ { NCNameTest } |
      qName ^^ { QNameTest }
    ) <~ ws
  def nodeType: Parser[NodeType] = ws ~> (
    "comment" ^^^ Comment |
      "text" ^^^ Text |
      "processing-instruction" ^^^ ProcessingInstruction |
      "node" ^^^ Node
    ) <~ ws
  def exprWhitespace: Parser[ExprWhitespace] = rep1(accept("Whitespace", { case Left(ch) if ExprWhitespace.allowedChars(ch) => ch })) ^^ { cs => ExprWhitespace(cs.mkString) }
  def ws: Parser[Unit] = opt(exprWhitespace) ^^^ { () }
  def qName: Parser[QName] = nCName ~ ':' ~ nCName ^^ { case p ~ _ ~ lp => QName(Some(p), lp) } | nCName ^^ { QName(None, _) }
  def nCName: Parser[NCName] = codePoint("NCName start code point", NCName.nameStartCodePoints) ~ rep(codePoint("NCName code point", NCName.nameCodePoints)) ^^ { case ch ~ cs => NCName(ch + cs.mkString) }
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
  def `(`: EParser[ExprToken] = ws ~ '(' ~ ws ^^^ reify{SimpleExprToken.`(`}
  def `)`: EParser[ExprToken] = ws ~ ')' ~ ws ^^^ reify{SimpleExprToken.`)`}
  def `[`: EParser[ExprToken] = ws ~ '[' ~ ws ^^^ reify{SimpleExprToken.`[`}
  def `]`: EParser[ExprToken] = ws ~ ']' ~ ws ^^^ reify{SimpleExprToken.`]`}
  def `..`: EParser[ExprToken] = ws ~ ".." ~ ws ^^^ reify{SimpleExprToken.`..`}
  def `.`: EParser[ExprToken] = ws ~ '.' ~ ws ^^^ reify{SimpleExprToken.`.`}
  def `@`: EParser[ExprToken] = ws ~ '@' ~ ws ^^^ reify{SimpleExprToken.`@`}
  def `,`: EParser[ExprToken] = ws ~ ',' ~ ws ^^^ reify{SimpleExprToken.`,`}
  def `::`: EParser[ExprToken] = ws ~ "::" ~ ws ^^^ reify{SimpleExprToken.`::`}

  def parseAll[T](p: Parser[T], in: Reader[Elem]): ParseResult[T] = phrase(p)(in)

  def apply(es: IndexedSeq[Either[Char, xc.Expr[Any]]]): ParseResult[xc.Expr[LocationPath]] = parseAll(locationPath, new SeqReader(es))
}

