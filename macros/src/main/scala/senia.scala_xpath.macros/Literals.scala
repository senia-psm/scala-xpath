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

trait Literals {
  val lc: Context
  import lc.universe.reify

  def literal(t: NodeType): lc.Expr[NodeType] = t match {
    case Comment => reify{ Comment }
    case Text => reify{ Text }
    case ProcessingInstruction => reify{ ProcessingInstruction }
    case Node => reify{ Node }
  }

  def literal(l: Literal): lc.Expr[Literal] = reify{ Literal(lc.literal(l.s).splice) }

  def literal(test: NodeTest): lc.Expr[NodeTest] = test match {
    case NodeTypeTest(t) => reify{ NodeTypeTest(literal(t).splice) }
    case InstructionNodeTest(l) => reify{ InstructionNodeTest(literal(l).splice) }
    case NCNameTest(n) => reify{ NCNameTest(literal(n).splice) }
    case NameTestAll => reify{ NameTestAll }
    case QNameTest(n) => reify{ QNameTest(literal(n).splice) }
  }

  def literal(name: AxisName): lc.Expr[AxisName] = name match {
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

  def literal(s: AxisSpecifier): lc.Expr[AxisSpecifier] = s match {
    case ElementAxis => reify{ ElementAxis }
    case AttributeAxis => reify{ AttributeAxis }
    case AxisSpecifierCommon(an) => reify{ AxisSpecifierCommon(literal(an).splice) }
  }

  def literal(d: Digits): lc.Expr[Digits] = reify{ Digits(lc.literal(d.s).splice) }
  implicit val digitsLiteral = literal(_: Digits)

  def literal(name: NCName): lc.Expr[NCName] = reify{ NCName(lc.literal(name.s).splice) }
  implicit val ncNameLiteral = literal(_: NCName)

  def literal[T <% lc.Expr[T]](o: Option[T]): lc.Expr[Option[T]] = o match {
    case Some(t) => reify{ Some((t: lc.Expr[T]).splice) }
    case None => reify{ None }
  }

  def literal(name: QName): lc.Expr[QName] = reify{ QName(literal(name.prefix).splice, literal(name.localPart).splice) }

  def literal(name: FunctionName): lc.Expr[FunctionName] = reify{ FunctionName(literal(name.n).splice) }

  def literal(o: AdditiveOperation): lc.Expr[AdditiveOperation] = o match {
    case Addition => reify{ Addition }
    case Subtraction => reify{ Subtraction }
  }
  implicit val additiveOperationLiteral = literal(_: AdditiveOperation)

  def literal(o: MultiplicativeOperator): lc.Expr[MultiplicativeOperator] = o match {
    case MultiplyOperator => reify{ MultiplyOperator }
    case Div => reify{ Div }
    case Mod => reify{ Mod }
  }
  implicit val multiplicativeOperatorLiteral = literal(_: MultiplicativeOperator)

  def literal(ss: StepSep): lc.Expr[StepSep] = ss match {
    case StepSepCommon => reify{ StepSepCommon }
    case AbbreviatedStepSep => reify{ AbbreviatedStepSep }
  }
  implicit val stepSepLiteral = literal(_: StepSep)

  def literal(e: Equality): lc.Expr[Equality] = e match {
    case Equal => reify{ Equal }
    case NotEqual => reify{ NotEqual }
  }
  implicit val equalityLiteral = literal(_: Equality)

  def literal(r: Relation): lc.Expr[Relation] = r match {
    case Less => reify{ Less }
    case LessOrEqual => reify{ LessOrEqual }
    case Greater => reify{ Greater }
    case GreaterOrEqual => reify{ GreaterOrEqual }
  }
  implicit val relationLiteral = literal(_: Relation)

  def literal(step: AbbreviatedStep): lc.Expr[AbbreviatedStep] = step match {
    case SelfStep => reify{ SelfStep }
    case ParentStep => reify{ ParentStep }
  }

  def seqExprToExprSeq[T](es: Seq[lc.Expr[T]]): lc.Expr[Seq[T]] = {
    @annotation.tailrec def loop(in: List[lc.Expr[T]], subRes: lc.Expr[List[T]] = reify{ Nil } ): lc.Expr[List[T]] =
      in match {
        case head :: tail => loop(tail, reify{ head.splice :: subRes.splice })
        case _ => subRes
      }

    loop( es.reverse.toList )
  }

  def seqPairToExprSeq[T1 <% lc.Expr[T1], T2](ts: Seq[(T1, lc.Expr[T2])]): lc.Expr[Seq[(T1, T2)]] =
    seqExprToExprSeq(ts.map{ case (t1, t2) => reify{ (t1: lc.Expr[T1]).splice -> t2.splice } })

}

