package senia.scala_xpath.macros

import scala.language.experimental.macros
import reflect.macros.Context
import senia.scala_xpath.model._
import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers
import util.matching.Regex
import util.parsing.input.{Position, Reader}

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */


object `package` {
  implicit class XPathContext(sc: StringContext) {
    def xp(as: Any*): LocationPath = macro xpathImpl
  }

  def xpathImpl(c: Context)(as: c.Expr[Any]*): c.Expr[LocationPath] = {
    import c.universe.{ Constant, Apply }

    class XPathParsers extends Parsers {
      import c.universe.reify

      class SeqReader(es: IndexedSeq[Either[Char, c.Expr[Any]]], init: Int = 0, line: Int = 1, column: Int = 1) extends Reader[Either[Char, c.Expr[Any]]]{
        self =>
        def first: Either[Char, c.Expr[Any]] = es(init)

        def rest: Reader[Either[Char, c.Expr[Any]]] =
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
                case _ => c.abort(c.enclosingPosition, "should never get here")
              }(collection.breakOut)
        }

        def atEnd: Boolean = init >= es.length
      }

      type Elem = Either[Char, c.Expr[Any]]

      type EParser[T] = Parser[c.Expr[T]]

      def stringToLeftSeq(s: String): List[Left[Char, c.Expr[Any]]] = augmentString(s).map{ Left(_) }(collection.breakOut)
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


      object L {

        def literal(t: NodeType): c.Expr[NodeType] = t match {
          case Comment => reify{ Comment }
          case Text => reify{ Text }
          case ProcessingInstruction => reify{ ProcessingInstruction }
          case Node => reify{ Node }
        }

        def literal(l: Literal): c.Expr[Literal] = reify{ Literal(c.literal(l.s).splice) }

        def literal(test: NodeTest): c.Expr[NodeTest] = test match {
          case NodeTypeTest(t) => reify{ NodeTypeTest(literal(t).splice) }
          case InstructionNodeTest(l) => reify{ InstructionNodeTest(literal(l).splice) }
          case NCNameTest(n) => reify{ NCNameTest(literal(n).splice) }
          case NameTestAll => reify{ NameTestAll }
          case QNameTest(n) => reify{ QNameTest(literal(n).splice) }
        }

        def literal(name: AxisName): c.Expr[AxisName] = name match {
          case Ancestor => reify{ Ancestor }
          case AncestorOrSelf => reify{ AncestorOrSelf }
          case Attribute => reify{ Attribute }
          case Child => reify{ Child }
          case Descendant => reify{ Descendant }
          case DescendantOrSelf => reify{ DescendantOrSelf }
          case Following => reify{ Following }
          case FollowingSibling => reify{ FollowingSibling }
          case Namespace => reify{ Namespace }
          case Parent => reify{ Parent }
          case Preceding => reify{ Preceding }
          case PrecedingSibling => reify{ PrecedingSibling }
          case Self => reify{ Self }
        }

        def literal(s: AxisSpecifier): c.Expr[AxisSpecifier] = s match {
          case ElementAxis => reify{ ElementAxis }
          case AttributeAxis => reify{ AttributeAxis }
          case AxisSpecifierCommon(an) => reify{ AxisSpecifierCommon(literal(an).splice) }
        }

        def literal(d: Digits): c.Expr[Digits] = reify{ Digits(c.literal(d.s).splice) }
        implicit val digitsLiteral = literal(_: Digits)

        def literal(name: NCName): c.Expr[NCName] = reify{ NCName(c.literal(name.s).splice) }
        implicit val ncNameLiteral = literal(_: NCName)

        def literal[T <% c.Expr[T]](o: Option[T]): c.Expr[Option[T]] = o match {
          case Some(t) => reify{ Some((t: c.Expr[T]).splice) }
          case None => reify{ None }
        }

        def literal(name: QName): c.Expr[QName] = reify{ QName(literal(name.prefix).splice, literal(name.localPart).splice) }

        def literal(name: FunctionName): c.Expr[FunctionName] = reify{ FunctionName(literal(name.n).splice) }

        def literal(o: AdditiveOperation): c.Expr[AdditiveOperation] = o match {
          case Addition => reify{ Addition }
          case Subtraction => reify{ Subtraction }
        }
        implicit val additiveOperationLiteral = literal(_: AdditiveOperation)

        def literal(o: MultiplicativeOperator): c.Expr[MultiplicativeOperator] = o match {
          case MultiplyOperator => reify{ MultiplyOperator }
          case Div => reify{ Div }
          case Mod => reify{ Mod }
        }
        implicit val multiplicativeOperatorLiteral = literal(_: MultiplicativeOperator)

        def literal(ss: StepSep): c.Expr[StepSep] = ss match {
          case StepSepCommon => reify{ StepSepCommon }
          case AbbreviatedStepSep => reify{ AbbreviatedStepSep }
        }
        implicit val stepSepLiteral = literal(_: StepSep)

        def literal(e: Equality): c.Expr[Equality] = e match {
          case Equal => reify{ Equal }
          case NotEqual => reify{ NotEqual }
        }
        implicit val equalityLiteral = literal(_: Equality)

        def literal(r: Relation): c.Expr[Relation] = r match {
          case Less => reify{ Less }
          case LessOrEqual => reify{ LessOrEqual }
          case Greater => reify{ Greater }
          case GreaterOrEqual => reify{ GreaterOrEqual }
        }
        implicit val relationLiteral = literal(_: Relation)

        def literal(step: AbbreviatedStep): c.Expr[AbbreviatedStep] = step match {
          case SelfStep => reify{ SelfStep }
          case ParentStep => reify{ ParentStep }
        }

        def seqExprToExprSeq[T](es: Seq[c.Expr[T]]): c.Expr[Seq[T]] = {
          @annotation.tailrec def loop(in: List[c.Expr[T]], subRes: c.Expr[List[T]] = reify{ Nil } ): c.Expr[List[T]] =
            in match {
              case head :: tail => loop(tail, reify{ head.splice :: subRes.splice })
              case _ => subRes
            }

          loop( es.reverse.toList )
        }

        def seqPairToExprSeq[T1 <% c.Expr[T1], T2](ts: Seq[(T1, c.Expr[T2])]): c.Expr[Seq[(T1, T2)]] =
          seqExprToExprSeq(ts.map{ case (t1, t2) => reify{ (t1: c.Expr[T1]).splice -> t2.splice } })
      }
      import L._

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
          case s ~ ss => reify{RelativeLocationPath(s.splice, L.seqPairToExprSeq(ss).splice) }
        }
      def stepSep: Parser[StepSep] = dblSlash ^^^ AbbreviatedStepSep | slash ^^^ StepSepCommon
      def step: EParser[Step] =
        abbreviatedStep ^^ { s => L.literal(s) } |
        axisSpecifier ~ nodeTest ~ rep(predicate) ^^ { case axs ~ nt ~ ps => reify{StepCommon(L.literal(axs).splice, L.literal(nt).splice, L.seqExprToExprSeq(ps).splice)} }
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
      def primaryExpr: EParser[PrimaryExpr] =
        accept("Right[c.Expr[Any]]", { case Right(e) => reify{ CustomVariableExpr(VariableReference(QName(None, NCName(c.literal(nextVarName).splice))), e.splice) } }) |
        variableReference ^^ { case VariableReference(n) => reify{ VariableExpr(VariableReference(L.literal(n).splice)) } } |
        `(` ~> expr <~ `)` ^^ { r => reify{GroupedExpr(r.splice)} } |
        functionCall ^^ { f => reify{ FunctionCallExpr(f.splice) } } |
        number ^^ { case Number(i, wd, f) => reify{ NumberExpr(Number(L.literal(i).splice, c.literal(wd).splice, L.literal(f).splice)) } } |
        literal ^^ { case l => reify{LiteralExpr(L.literal(l).splice)} }
      def functionCall: EParser[FunctionCall] = functionName ~ `(` ~ repsep(argument, `,`) <~ `)` ^^ {
        case fn ~ _ ~ ags => reify{ FunctionCall(L.literal(fn).splice, L.seqExprToExprSeq(ags).splice) }
      }
      def argument: EParser[Argument] = expr ^^ { e => reify{ Argument(e.splice) } }
      def unionExpr: EParser[UnionExpr] = rep1sep(pathExpr, ws ~> '|' <~ ws) ^^ { es => reify{UnionExpr(L.seqExprToExprSeq(es).splice)} }
      def pathExpr: EParser[PathExpr] =
        locationPath ^^ { lp => reify{LocationPathExpr(lp.splice)} } |||
          filterExpr ~ opt(( dblSlash | slash ) ~ relativeLocationPath) ^^ {
            case f ~ None => reify{FilterPathExpr(f.splice)}
            case f ~ Some(s ~ rp) => reify{FilterPathExpr(f.splice, Some(rp.splice), c.literal(s).splice == "//")}
          }
      def filterExpr: EParser[FilterExpr] = primaryExpr ~ rep(predicate) ^^ { case pe ~ ps => reify{FilterExpr(pe.splice, L.seqExprToExprSeq(ps).splice)} }
      def orExpr: EParser[OrExpr] = rep1sep(andExpr, ws ~> "or" <~ ws) ^^ { es => reify{OrExpr(L.seqExprToExprSeq(es).splice)} }
      def andExpr: EParser[AndExpr] = rep1sep(equalityExpr, ws ~> "and" <~ ws) ^^ { es => reify{AndExpr(L.seqExprToExprSeq(es).splice)} }
      def equalityExpr: EParser[EqualityExpr] = relationalExpr ~ rep( equality ~ relationalExpr ^^ { case e ~ r => e -> r } ) ^^ { case r ~ rs => reify{EqualityExpr(r.splice, L.seqPairToExprSeq(rs).splice)} }
      def equality: Parser[Equality] = ws ~> ( '=' ^^^ Equal | "!=" ^^^ NotEqual ) <~ ws
      def relationalExpr: EParser[RelationalExpr] = additiveExpr ~ rep( relation ~ additiveExpr ^^ { case r ~ a => r -> a } ) ^^ { case ae ~ aes => reify{RelationalExpr(ae.splice, L.seqPairToExprSeq(aes).splice)} }
      def relation: Parser[Relation] = ws ~> ( "<=" ^^^ LessOrEqual | '<' ^^^ Less | ">=" ^^^ GreaterOrEqual | '>' ^^^ Greater ) <~ ws
      def additiveExpr: EParser[AdditiveExpr] = multiplicativeExpr ~ rep( additiveOperation ~ multiplicativeExpr ^^ { case ao ~ me => ao -> me } ) ^^ {
        case me ~ ms => reify{AdditiveExpr(me.splice, L.seqPairToExprSeq(ms).splice)}
      }
      def additiveOperation: Parser[AdditiveOperation] = ws ~> '+' <~ ws ^^^ Addition | ws ~> '-' <~ ws ^^^ Subtraction


      def multiplicativeExpr: EParser[MultiplicativeExpr] = unaryExpr ~ rep( multiplicativeOperator ~ unaryExpr ^^ {
        case o ~ e => o -> e } ) ^^ { case ue ~ us => reify{MultiplicativeExpr(ue.splice, L.seqPairToExprSeq(us).splice)}
      }
      def multiplicativeOperator: Parser[MultiplicativeOperator] = multiplyOperator | ws ~> "div" <~ ws ^^^ Div | ws ~> "mod" <~ ws ^^^ Mod
      def multiplyOperator: Parser[MultiplyOperator.type] = ws ~> '*' <~ ws ^^^ MultiplyOperator
      def unaryExpr: EParser[UnaryExpr] = rep(ws ~> '-' <~ ws) ~ unionExpr ^^ { case ms ~ u => reify{UnaryExpr(u.splice, L.seqExprToExprSeq(ms.map{ _ => reify{ Subtraction }}).splice )}}
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

      def apply(es: IndexedSeq[Either[Char, c.Expr[Any]]]): ParseResult[c.Expr[LocationPath]] = parseAll(locationPath, new SeqReader(es))
    }

    val strings = c.prefix.tree match {
      case Apply(_, List(Apply(_, ss))) => ss
      case _ => c.abort(c.enclosingPosition, "not a interpolation of XPath. Cannot extract parts.")
    }

    val chars = strings.map{
      case c.universe.Literal(Constant(source: String)) => source.map{ Left(_) }
      case _ => c.abort(c.enclosingPosition, "not a interpolation of XPath. Cannot extract string.")
    }

    if (chars.length == 0)
      c.abort(c.enclosingPosition, "too few parts")
    if (as.length < chars.tail.length)
      c.abort(c.enclosingPosition, "too few arguments for interpolated XPath")
    if (as.length > chars.tail.length)
      c.abort(c.enclosingPosition, "too many arguments for interpolated XPath")

    val source =
      chars.head ++ (as zip chars.tail).
        flatMap{ case (a, cs) => { Right(a) +: cs } }

    val parser = new XPathParsers
    val result = parser(source) match {
      case parser.Success(r: c.Expr[LocationPath], next) if next.atEnd => r
      case parser.Failure(msg, next) => c.abort(c.enclosingPosition, msg)
      case parser.Error(msg, next) => c.abort(c.enclosingPosition, msg)
      case _ => c.abort(c.enclosingPosition, "should never get here")
    }
    result
  }
}