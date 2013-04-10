package senia.scala_xpath.model
/*
[1]       LocationPath       ::=       RelativeLocationPath    
            | AbsoluteLocationPath
*/
sealed trait LocationPath extends PathExpr

/* [2]       AbsoluteLocationPath       ::=       '/' RelativeLocationPath?    
            | AbbreviatedAbsoluteLocationPath */
sealed trait AbsoluteLocationPath extends LocationPath
case class AbsoluteLocationPathCommon(p: Option[RelativeLocationPath]) extends AbsoluteLocationPath {
  override def toString = "/" + p.mkString
  def variables: Map[QName, Any] = p.seq.flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = p.seq.flatMap{ _.functions }(collection.breakOut)
}
/* [10]       AbbreviatedAbsoluteLocationPath       ::=       '//' RelativeLocationPath */
case class AbbreviatedAbsoluteLocationPath(p: RelativeLocationPath) extends AbsoluteLocationPath {
  override def toString = "//" + p
  def variables: Map[QName, Any] = p.variables
  def functions: Map[(QName, Int), (List[Any]) => Any] = p.functions
}

/* [3]       RelativeLocationPath       ::=       Step    
            | RelativeLocationPath '/' Step    
            | AbbreviatedRelativeLocationPath    */
case class RelativeLocationPath(s: Step, ss: Seq[(StepSep, Step)]) extends LocationPath {
  override def toString = "" + s + ss.map{ case (sep, st) => "" + sep + st}.mkString
  def variables: Map[QName, Any] = s.variables ++ ss.flatMap{ case (a, b) =>  b.variables }
  def functions: Map[(QName, Int), (List[Any]) => Any] = s.functions ++ ss.flatMap{ case (a, b) =>  b.functions }
}
sealed trait StepSep
case object StepSepCommon extends StepSep { override def toString = "/" }
case object AbbreviatedStepSep extends StepSep { override def toString = "//" }
// case class RelativeLocationPathCommon(p: Option[RelativeLocationPath], s: Step) extends RelativeLocationPath { override def toString() = p.map{ _ + "/" }.mkString + s }
/* [11]       AbbreviatedRelativeLocationPath       ::=       RelativeLocationPath '//' Step    */
// case class AbbreviatedRelativeLocationPath(p: RelativeLocationPath, s: Step) extends RelativeLocationPath { override def toString() = p + "//" + s }

/* [4]       Step       ::=       AxisSpecifier NodeTest Predicate*    
            | AbbreviatedStep */
sealed trait Step {
  def variables: Map[QName, Any]
  def functions: Map[(QName, Int), (List[Any]) => Any]
}
case class StepCommon(as: AxisSpecifier, nt: NodeTest, ps: Seq[Predicate]) extends Step {
  override def toString = "" + as + nt + ps.mkString
  def variables: Map[QName, Any] = ps.flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = ps.flatMap{ _.functions }(collection.breakOut)
}
/* [12]       AbbreviatedStep       ::=       '.'
            | '..' */
sealed trait AbbreviatedStep extends Step
case object SelfStep extends AbbreviatedStep {
  override def toString = "."
  def variables: Map[QName, Any] = Map()
  def functions: Map[(QName, Int), (List[Any]) => Any] = Map()
}
case object ParentStep extends AbbreviatedStep {
  override def toString = ".."
  def variables: Map[QName, Any] = Map()
  def functions: Map[(QName, Int), (List[Any]) => Any] = Map()
}

/* [5]       AxisSpecifier       ::=       AxisName '::'    
            | AbbreviatedAxisSpecifier */
sealed trait AxisSpecifier
case class AxisSpecifierCommon(an: AxisName) extends AxisSpecifier { override def toString = an + "::" }
/* [13]       AbbreviatedAxisSpecifier       ::=       '@'? */
sealed trait AbbreviatedAxisSpecifier extends AxisSpecifier
case object ElementAxis extends AbbreviatedAxisSpecifier { override def toString = "" }
case object AttributeAxis extends AbbreviatedAxisSpecifier { override def toString = "@" }


/* [6]       AxisName       ::=       'ancestor'    
            | 'ancestor-or-self'    
            | 'attribute'    
            | 'child'    
            | 'descendant'    
            | 'descendant-or-self'    
            | 'following'    
            | 'following-sibling'    
            | 'namespace'    
            | 'parent'    
            | 'preceding'    
            | 'preceding-sibling'    
            | 'self' */
sealed abstract class AxisName(s: String) extends ExprToken { override def toString = s }
case object Ancestor extends AxisName("ancestor")
case object AncestorOrSelf extends AxisName("ancestor-or-self")
case object Attribute extends AxisName("attribute")
case object Child extends AxisName("child")
case object Descendant extends AxisName("descendant")
case object DescendantOrSelf extends AxisName("descendant-or-self")
case object Following extends AxisName("following")
case object FollowingSibling extends AxisName("following-sibling")
case object Namespace extends AxisName("namespace")
case object Parent extends AxisName("parent")
case object Preceding extends AxisName("preceding")
case object PrecedingSibling extends AxisName("preceding-sibling")
case object Self extends AxisName("self")

/* [7]       NodeTest       ::=       NameTest    
            | NodeType '(' ')'    
            | 'processing-instruction' '(' Literal ')'     */
sealed trait NodeTest

case class NodeTypeTest(t: NodeType) extends NodeTest { override def toString = t + "()" }
case class InstructionNodeTest(l: Literal) extends NodeTest { override def toString = s"processing-instruction($l)" }

/* [8]       Predicate       ::=       '[' PredicateExpr ']' */
case class Predicate(pe: PredicateExpr) {
  override def toString = s"[$pe]"
  def variables: Map[QName, Any] = pe.variables
  def functions: Map[(QName, Int), (List[Any]) => Any] = pe.functions
}

/* [9]       PredicateExpr       ::=       Expr */
case class PredicateExpr(e: Expr) {
  override def toString = e.toString
  def variables: Map[QName, Any] = e.variables
  def functions: Map[(QName, Int), (List[Any]) => Any] = e.functions
}

/* [14]       Expr       ::=       OrExpr    */
case class Expr(e: OrExpr) {
  override def toString = e.toString
  def variables: Map[QName, Any] = e.variables
  def functions: Map[(QName, Int), (List[Any]) => Any] = e.functions
}

/* [15]       PrimaryExpr       ::=       VariableReference
            | '(' Expr ')'    
            | Literal    
            | Number    
            | FunctionCall */
sealed trait PrimaryExpr
sealed trait CustomVariableExpr[T] extends PrimaryExpr {
  def vr: VariableReference
  def value: T
  override def toString = vr.toString
  def asValue: Any
}
object CustomVariableExpr { def unapply(e: CustomVariableExpr[_]): Option[(VariableReference, Any)] = Some(e.vr -> e.asValue) }
case class CustomIntVariableExpr(vr: VariableReference, value: BigInt) extends CustomVariableExpr[BigInt] { val asValue = value.toString() }
case class CustomDoubleVariableExpr(vr: VariableReference, value: Double) extends CustomVariableExpr[Double] { val asValue = value: java.lang.Double }
case class CustomStringVariableExpr(vr: VariableReference, value: String) extends CustomVariableExpr[String] { val asValue = value }
case class GroupedExpr(e: Expr) extends PrimaryExpr { override def toString = s"($e)" }

case class CustomFunction(n: QName, arity: Int, f: PartialFunction[(List[Any]), Any]){
  override def toString = n.toString
}
object CustomFunction{
  def apply0(n: QName, f0: () => Any): CustomFunction = CustomFunction(n, 0, { case Nil => f0() })
  def apply1(n: QName, f1: (Any) => Any): CustomFunction = CustomFunction(n, 1, { case a1 :: Nil => f1(a1) })
  def apply2(n: QName, f2: (Any, Any) => Any): CustomFunction = CustomFunction(n, 2, { case a1 :: a2 :: Nil => f2(a1, a2) })
  def apply3(n: QName, f3: (Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 3, { case a1 :: a2 :: a3 :: Nil => f3(a1, a2, a3) })
  def apply4(n: QName, f4: (Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 4, { case a1 :: a2 :: a3 :: a4 :: Nil => f4(a1, a2, a3, a4) })
  def apply5(n: QName, f5: (Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 5, { case a1 :: a2 :: a3 :: a4 :: a5 :: Nil => f5(a1, a2, a3, a4, a5) })
  def apply6(n: QName, f6: (Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 6, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: Nil => f6(a1, a2, a3, a4, a5, a6) })
  def apply7(n: QName, f7: (Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 7, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: Nil => f7(a1, a2, a3, a4, a5, a6, a7) })
  def apply8(n: QName, f8: (Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 8, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: Nil => f8(a1, a2, a3, a4, a5, a6, a7, a8) })
  def apply9(n: QName, f9: (Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 9, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: Nil => f9(a1, a2, a3, a4, a5, a6, a7, a8, a9) })
  def apply10(n: QName, f10: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 10, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: Nil => f10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) })
  def apply11(n: QName, f11: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 11, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: Nil => f11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) })
  def apply12(n: QName, f12: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 12, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: Nil => f12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) })
  def apply13(n: QName, f13: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 13, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: Nil => f13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) })
  def apply14(n: QName, f14: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 14, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: Nil => f14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) })
  def apply15(n: QName, f15: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 15, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: Nil => f15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) })
  def apply16(n: QName, f16: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 16, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: Nil => f16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) })
  def apply17(n: QName, f17: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 17, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: Nil => f17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) })
  def apply18(n: QName, f18: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 18, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: Nil => f18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) })
  def apply19(n: QName, f19: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 19, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: a19 :: Nil => f19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) })
  def apply20(n: QName, f20: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 20, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: a19 :: a20 :: Nil => f20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) })
  def apply21(n: QName, f21: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 21, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: a19 :: a20 :: a21 :: Nil => f21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) })
  def apply22(n: QName, f22: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any): CustomFunction = CustomFunction(n, 22, { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: a19 :: a20 :: a21 :: a22 :: Nil => f22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) })
}
case class CustomFunctionCall(f: CustomFunction, as: Seq[Argument]) extends PrimaryExpr { override def toString = s"$f(${ as mkString "," })" }


/* [16]       FunctionCall       ::=       FunctionName '(' ( Argument ( ',' Argument )* )? ')' */
case class FunctionCall(n: FunctionName, as: Seq[Argument]) extends PrimaryExpr { override def toString = s"$n(${ as mkString "," })" }
/* [17]       Argument       ::=       Expr */
case class Argument(e: Expr) { override def toString = e.toString }

/* [18]       UnionExpr       ::=       PathExpr    
            | UnionExpr '|' PathExpr    */
case class UnionExpr(ps: Seq[PathExpr]) { // TODO: NEL
  require(ps.nonEmpty, "At least 1 PathExpr")
  override def toString = ps.mkString(" | ")
  def variables: Map[QName, Any] = ps.flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = ps.flatMap{ _.functions }(collection.breakOut)
}

/* [19]       PathExpr       ::=       LocationPath    
            | FilterExpr    
            | FilterExpr '/' RelativeLocationPath    
            | FilterExpr '//' RelativeLocationPath    */
sealed trait PathExpr {
  def variables: Map[QName, Any]
  def functions: Map[(QName, Int), (List[Any]) => Any]
}
case class FilterPathExpr(f: FilterExpr, rp: Option[RelativeLocationPath] = None, doubleSlash: Boolean = false) extends PathExpr {
  override def toString = f + rp.map{ _ + (if(doubleSlash) "//" else "/") }.mkString
  def variables: Map[QName, Any] = f.variables ++ rp.seq.flatMap{ _.variables }
  def functions: Map[(QName, Int), (List[Any]) => Any] = f.functions ++ rp.seq.flatMap{ _.functions }
}

/* [20]       FilterExpr       ::=       PrimaryExpr    
            | FilterExpr Predicate */
case class FilterExpr(pe: PrimaryExpr, ps: Seq[Predicate] = Seq()) {
  override def toString = pe + ps.mkString
  def variables: Map[QName, Any] = Some(pe).collect{ case CustomVariableExpr(VariableReference(k), v) => k -> v}.toMap
  def functions: Map[(QName, Int), (List[Any]) => Any] = Some(pe).collect{ case CustomFunctionCall(CustomFunction(n, arity, f), _) => n -> arity -> f}.toMap
}

/* [21]       OrExpr       ::=       AndExpr
            | OrExpr 'or' AndExpr    */
case class OrExpr(ands: Seq[AndExpr]) {
  require(ands.nonEmpty, "At least 1 AndExpr")
  override def toString = ands.mkString(" or ")
  def variables: Map[QName, Any] = ands.flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = ands.flatMap{ _.functions }(collection.breakOut)
}

/* [22]       AndExpr       ::=       EqualityExpr    
            | AndExpr 'and' EqualityExpr    */
case class AndExpr(eqs: Seq[EqualityExpr]) {
  require(eqs.nonEmpty, "At least 1 EqualityExpr")
  override def toString = eqs.mkString(" and ")
  def variables: Map[QName, Any] = eqs.flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = eqs.flatMap{ _.functions }(collection.breakOut)
}

/* [23]       EqualityExpr       ::=       RelationalExpr    
            | EqualityExpr '=' RelationalExpr    
            | EqualityExpr '!=' RelationalExpr    */
case class EqualityExpr(rel: RelationalExpr, rs: Seq[(Equality, RelationalExpr)]) {
  override def toString = rel + rs.map{ case (e, r) => s" $e $r" }.mkString
  def variables: Map[QName, Any] = (rel +: rs.map{ case (a, b) => b }).flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = (rel +: rs.map{ case (a, b) => b }).flatMap{ _.functions }(collection.breakOut)
}
sealed abstract class Equality(s: String) extends Operator { override def toString = s }
case object Equal extends Equality("=")
case object NotEqual extends Equality("!=")

/* [24]       RelationalExpr       ::=       AdditiveExpr    
            | RelationalExpr '<' AdditiveExpr    
            | RelationalExpr '>' AdditiveExpr    
            | RelationalExpr '<=' AdditiveExpr    
            | RelationalExpr '>=' AdditiveExpr    */
case class RelationalExpr(ae: AdditiveExpr, as: Seq[(Relation, AdditiveExpr)]) {
  override def toString = ae + as.map{ case (r, a) => s" $r $a" }.mkString
  def variables: Map[QName, Any] = (ae +: as.map{ case (a, b) => b }).flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = (ae +: as.map{ case (a, b) => b }).flatMap{ _.functions }(collection.breakOut)
}
sealed abstract class Relation(s: String) extends Operator { override def toString = s }
case object Less extends Relation("<")
case object LessOrEqual extends Relation("<=")
case object Greater extends Relation(">")
case object GreaterOrEqual extends Relation(">=")

/* [25]       AdditiveExpr       ::=       MultiplicativeExpr    
            | AdditiveExpr '+' MultiplicativeExpr    
            | AdditiveExpr '-' MultiplicativeExpr    */
case class AdditiveExpr(me: MultiplicativeExpr, ms: Seq[(AdditiveOperation, MultiplicativeExpr)]) {
  override def toString = me + ms.map{ case (ao, m) => s" $ao $m" }.mkString
  def variables: Map[QName, Any] = (me +: ms.map{ case (a, b) => b }).flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = (me +: ms.map{ case (a, b) => b }).flatMap{ _.functions }(collection.breakOut)
}
sealed abstract class AdditiveOperation(s: String) extends Operator { override def toString = s }
case object Addition extends AdditiveOperation("+")
case object Subtraction extends AdditiveOperation("-")

/* [26]       MultiplicativeExpr       ::=       UnaryExpr    
            | MultiplicativeExpr MultiplyOperator UnaryExpr    
            | MultiplicativeExpr 'div' UnaryExpr    
            | MultiplicativeExpr 'mod' UnaryExpr    */
case class MultiplicativeExpr(ue: UnaryExpr, us: Seq[(MultiplicativeOperator, UnaryExpr)]) {
  override def toString = ue + us.map{ case (o, e) => s" $o e" }.mkString
  def variables: Map[QName, Any] = (ue +: us.map{ case (a, b) => b }).flatMap{ _.variables }(collection.breakOut)
  def functions: Map[(QName, Int), (List[Any]) => Any] = (ue +: us.map{ case (a, b) => b }).flatMap{ _.functions }(collection.breakOut)
}
sealed abstract class MultiplicativeOperator(s: String) extends Operator { override def toString = s }
/* [34]       MultiplyOperator       ::=       '*' */
case object MultiplyOperator extends MultiplicativeOperator("*")
case object Div extends MultiplicativeOperator("div") with OperatorName
case object Mod extends MultiplicativeOperator("mod") with OperatorName

/* [27]       UnaryExpr       ::=       UnionExpr    
            | '-' UnaryExpr */
case class UnaryExpr(union: UnionExpr, minuses: Seq[Subtraction.type] = Nil) {
  override def toString = minuses.mkString + union
  def variables: Map[QName, Any] = union.variables
  def functions: Map[(QName, Int), (List[Any]) => Any] = union.functions
}

/* [28]       ExprToken       ::=       '(' | ')' | '[' | ']' | '.' | '..' | '@' | ',' | '::'    
            | NameTest    
            | NodeType    
            | Operator    
            | FunctionName    
            | AxisName    
            | Literal    
            | Number    
            | VariableReference    */
sealed trait ExprToken
sealed abstract class SimpleExprToken(s: String) extends ExprToken { override def toString = s }
object SimpleExprToken {
  case object `(` extends SimpleExprToken("(")
  case object `)` extends SimpleExprToken(")")
  case object `[` extends SimpleExprToken("[")
  case object `]` extends SimpleExprToken("]")
  case object `..` extends SimpleExprToken("..")
  case object `.` extends SimpleExprToken(".")
  case object `@` extends SimpleExprToken("@")
  case object `,` extends SimpleExprToken(",")
  case object `::` extends SimpleExprToken("::")
}

/* [29]       Literal       ::=       '"' [^"]* '"'    
            | "'" [^']* "'"    */
case class Literal(s: String) extends ExprToken with PrimaryExpr {
  val Format1 = """"[^"]*"""".r //"
  val Format2 = """'[^']*'""".r
  require(
    s match {
      case Format1() => true
      case Format2() => true
      case _ => false
    },
    """Literal format is "[^"]*" or '[^']*'""" //"
  )
  override def toString = s
}

/* [30]       Number       ::=       Digits ('.' Digits?)?    
            | '.' Digits    */
case class Number(integral: Option[Digits] = None, withDot: Boolean = false, fractional: Option[Digits] = None) extends ExprToken with PrimaryExpr {
  require(
    None != (integral orElse fractional),
    "Integral or fractional part is required."
  )
  require(
    None == fractional || withDot,
    "Dot is required for fractional part."
  )
  override def toString = integral.mkString + (if(withDot) "." else "") + fractional.mkString
}

/* [31]       Digits       ::=       [0-9]+    */
case class Digits(s: String) {
  val Format = """[0-9]+""".r //"
  require(
    s match {
      case Format() => true
      case _ => false
    },
    """Digits format is [0-9]+""" //"
  )
  override def toString = s
}

/* [32]       Operator       ::=       OperatorName    
            | MultiplyOperator    
            | '/' | '//' | '|' | '+' | '-' | '=' | '!=' | '<' | '<=' | '>' | '>='    */
sealed trait Operator extends ExprToken
case object `/` extends Operator { override def toString = "/" }
case object `//` extends Operator { override def toString = "//" }
case object Pipeline extends Operator { override def toString = "|" }

/* [33]       OperatorName       ::=       'and' | 'or' | 'mod' | 'div'    */
sealed trait OperatorName extends Operator
case object And extends OperatorName { override def toString = "and" }
case object Or extends OperatorName { override def toString = "or" }

/* [35]       FunctionName       ::=       QName - NodeType     */
case class FunctionName(n: QName) extends ExprToken {
  private[this] val s = n.toString
  require(s != Comment.toString && s != Text.toString && s != ProcessingInstruction.toString && s != Node.toString, "Not valid NodeType")
  override def toString = s
}

/* [36]       VariableReference       ::=       '$' QName    */
case class VariableReference(n: QName) extends ExprToken with PrimaryExpr { override def toString = "$" + n.toString }

/* [37]       NameTest       ::=       '*'    
            | NCName ':' '*'    
            | QName */
sealed abstract class NameTest extends NodeTest with ExprToken
case object NameTestAll extends NameTest { override def toString = "*" }
case class NCNameTest(name: NCName) extends NameTest { override def toString = name + ":*" }
case class QNameTest(name: QName) extends NameTest { override def toString = name.toString }

/* [38]       NodeType       ::=       'comment'    
            | 'text'    
            | 'processing-instruction'    
            | 'node'    */
sealed abstract class NodeType(s: String) extends ExprToken { override def toString = s }
object NodeType {
  val all = Set(Comment, Text, ProcessingInstruction, Node)
}
case object Comment extends NodeType("comment")
case object Text extends NodeType("text")
case object ProcessingInstruction extends NodeType("processing-instruction")
case object Node extends NodeType("node")

/* [39]       ExprWhitespace       ::=       S //S       ::=       (#x20 | #x9 | #xD | #xA)+ */
case class ExprWhitespace(s: String) {
  require(s.length > 0, "Not empty")
  require(s.forall{ExprWhitespace.allowedChars}, "Whitespace chars only")
  override def toString = s
}
object ExprWhitespace {
  val allowedChars = Set(0x20, 0x9, 0xD, 0xA).map{ _.toChar }
}

//XML 1.0:

/* [7]       QName       ::=       PrefixedName
            | UnprefixedName
[8]       PrefixedName       ::=       Prefix ':' LocalPart
[9]       UnprefixedName       ::=       LocalPart
[10]       Prefix       ::=       NCName
[11]       LocalPart       ::=       NCName */
case class QName(prefix: Option[NCName], localPart: NCName) { override def toString = prefix.map{ _ + ":" }.mkString + localPart }
object QName {
  def isValid(s: String): Boolean = s.nonEmpty && s.count{ _ == ':' } <= 1 && s.split(":").forall{ NCName.isValid }
}

/* [4]       NCName       ::=       Name - (Char* ':' Char*)    ( An XML Name, minus the ":" ) */
case class NCName(s: String) {
  require(NCName.isValid(s), s"Valid NCName, found: '$s'")
  override def toString = s
}
object NCName {
  @annotation.tailrec def UTF32point(s: String, idx: Int = 0, found: List[Int] = Nil): List[Int] = {
    if (idx >= s.length) found.reverse
    else {
      val point = s.codePointAt(idx)
      UTF32point(s, idx + Character.charCount(point), point :: found)
    }
  }

  val nameStartCodePoints =
//    Set(':') ++
    ('A' to 'Z').map{ _.toInt }.toSet ++
    Set('_'.toInt) ++
    ('a' to 'z').map{ _.toInt } ++
    (
      (0xC0 to 0xD6) ++
      (0xD8 to 0xF6) ++
      (0xF8 to 0x2FF) ++
      (0x370 to 0x37D) ++
      (0x37F to 0x1FFF) ++
      (0x200C to 0x200D) ++
      (0x2070 to 0x218F) ++
      (0x2C00 to 0x2FEF) ++
      (0x3001 to 0xD7FF) ++
      (0xF900 to 0xFDCF) ++
      (0xFDF0 to 0xFFFD) ++
      (0x10000 to 0xEFFFF)
    ) // http://stackoverflow.com/questions/5052042/how-to-split-strings-into-characters-in-scala
  // f"${655555}%c" codePointAt 0
  val nameCodePoints =
    nameStartCodePoints ++
    Set('-', '.').map{ _.toInt } ++
    ('0' to '9').map{ _.toInt } ++
    Set(0xB7) ++
    ((0x0300 to 0x036F) ++ (0x203F to 0x2040))
  def isValid(s: String): Boolean = s.length > 0 && nameStartCodePoints(s.codePointAt(0)) && UTF32point(s.drop(Character.charCount(s.codePointAt(0)))).forall{ nameCodePoints }
}
