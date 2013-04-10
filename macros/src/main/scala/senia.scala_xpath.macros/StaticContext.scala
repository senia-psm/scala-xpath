package senia.scala_xpath.macros

import scala.language.experimental.macros
import senia.scala_xpath.model.LocationPath
import reflect.macros.Context

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

object StaticContext {
  implicit class XPathContext(sc: StringContext) {
    def x(as: Any*): LocationPath = macro xpathImpl
  }

  def xpathImpl(c: Context)(as: c.Expr[Any]*): c.Expr[LocationPath] = {
    import c.universe.{ Try => _, _ }

    val Apply(_, List(Apply(_, List(Literal(Constant(source: String)))))) = c.prefix.tree

    val parser = new senia.scala_xpath.parser.XPathParsers
    val result = parser(source).get
    c.Expr[LocationPath](Literal(Constant(result)))
  }
}
