package senia.scala_xpath.macros

import scala.language.experimental.macros
import reflect.macros.Context
import senia.scala_xpath.model._
import scala.language.implicitConversions

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
    val parser = new { val xc: c.type = c } with XPathParsers
    val result = parser(source) match {
      case parser.Success(r: c.Expr[LocationPath], next) if next.atEnd => r
      case parser.Failure(msg, next) => c.abort(c.enclosingPosition, msg)
      case parser.Error(msg, next) => c.abort(c.enclosingPosition, msg)
      case _ => c.abort(c.enclosingPosition, "should never get here")
    }
    result
  }
}