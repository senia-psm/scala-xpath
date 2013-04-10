package senia.scala_xpath.model

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */

import collection.breakOut
import javax.xml.xpath.{XPathExpression, XPathConstants, XPathFactory, XPathVariableResolver}
import xml.NodeSeq
import javax.xml.namespace.{QName => JQNamr}
import org.w3c.dom.{NodeList, Document}
import javax.xml.parsers.DocumentBuilderFactory
import java.io.StringReader
import org.xml.sax.InputSource
import com.sun.org.apache.xalan.internal.xsltc.trax.DOM2SAX
import scala.xml.parsing.NoBindingFactoryAdapter

object `package` {
  private def locationPathToExpression(p: LocationPath): XPathExpression = {
    val factory = XPathFactory.newInstance()
    factory.setXPathVariableResolver(
      new XPathVariableResolver {
        def resolveVariable(variableName: JQNamr): AnyRef = {
          if (variableName == null)
            throw new NullPointerException("The variable name cannot be null")

          val prefix = if (variableName.getPrefix.nonEmpty) Some(NCName(variableName.getPrefix)) else None

          p.variables.
            get(QName(prefix, NCName(variableName.getLocalPart))).
            map{_.asInstanceOf[AnyRef]}.
            getOrElse(null)
        }})
    factory.newXPath().compile(p.toString)
  }

  implicit class NodeListIndexedSeq(nl: NodeList) extends IndexedSeq[org.w3c.dom.Node] {
    val length = nl.getLength()
    def apply(idx: Int) = nl.item(idx)
    override val hasDefiniteSize = true
  }

  implicit class W3CNodeXPathHelper(val node: org.w3c.dom.Node) extends AnyVal {
    private def applyXPath(p: LocationPath): NodeList =
      locationPathToExpression(p).
        evaluate(node, XPathConstants.NODESET).
        asInstanceOf[NodeList]

    def \(p: RelativeLocationPath): NodeList = applyXPath(AbsoluteLocationPathCommon(Some(p)))
    def \\(p: RelativeLocationPath): NodeList = applyXPath(AbbreviatedAbsoluteLocationPath(p))
  }

  implicit class W3CNodeListXPathHelper(val nl: NodeList) extends AnyVal {
    private def applyXPath(p: LocationPath): NodeList =
      locationPathToExpression(p).
        evaluate(nl, XPathConstants.NODESET).
        asInstanceOf[NodeList]

    def \(p: RelativeLocationPath): NodeList = applyXPath(AbsoluteLocationPathCommon(Some(p)))
    def \\(p: RelativeLocationPath): NodeList = applyXPath(AbbreviatedAbsoluteLocationPath(p))
  }

  implicit class XPathHelper(val ns: NodeSeq) extends AnyVal {
    private def applyXPath(p: LocationPath): NodeSeq = {
      val expression = locationPathToExpression(p)

      def nodeToDocument(n: xml.Node): Document = DocumentBuilderFactory.
        newInstance().
        newDocumentBuilder().
        parse(new InputSource(new StringReader(n.toString())))

      def asXml(dom: org.w3c.dom.Node): xml.Node = {
        val dom2sax = new DOM2SAX(dom)
        val adapter = new NoBindingFactoryAdapter
        dom2sax.setContentHandler(adapter)
        dom2sax.parse()
        adapter.rootElem
      }
      def apply(n: xml.Node): NodeSeq = {
        val nodeList = expression.
          evaluate(nodeToDocument(n), XPathConstants.NODESET).
          asInstanceOf[NodeList]

        nodeList.map{ asXml }(breakOut)
      }

      ns match {
        case n : xml.Node => apply(n)
        case _ => ns.flatMap(apply)
      }
    }

    def \(p: RelativeLocationPath): NodeSeq = applyXPath(AbsoluteLocationPathCommon(Some(p)))
    def \\(p: RelativeLocationPath): NodeSeq = applyXPath(AbbreviatedAbsoluteLocationPath(p))
  }
}
