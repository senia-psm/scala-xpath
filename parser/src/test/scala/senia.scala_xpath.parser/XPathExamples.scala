package senia.scala_xpath.parser

/**
 * Copyright (c) 2013. Semjon Popugaev (senia).
 * Licensed under the GPLv3.
 * See the LICENSE file for details.
 */
object XPathExamples {
  val examples = Seq(
    """./author""", // Все элементы <author> текущего контекста. Заметьте, что это эквивалентно выражению в следующей строке.
    """author""", // Все элементы <author> текущего контекста.
    """first.name""", // Все элементы <first.name> текущего контекста.
    """/bookstore""", // Элемент документа (<bookstore>) данного документа.
    """//author""", // Все элементы <author> данного документа.
    """book[/bookstore/@specialty=@style]""", // Все элементы <book>, для которых значение атрибута style равно значению атрибута specialty элемента <bookstore> корневого узла документа.
    """author/first-name""", // Все элементы <first-name>, являющиеся дочерними для элемента <author>.
    """bookstore//title""", // Все элементы <title> на первом или более глубоких уровнях элемента <bookstore> (потомки произвольного уровня). Заметьте, что это отличается от выражения в следующей строке.
    """bookstore/*/title""", // Все элементы <title>, являющиеся «внуками» элементов <bookstore>.
    """bookstore//book/excerpt//emph""", // Все элементы <emph> в любом месте внутри элементов <excerpt>, дочерних по отношению к элементам <book>, находящимся в любом месте внутри элемента <bookstore>.
    """.//title""", // Все элементы <title> на первом или более глубоких уровнях текущего контекста. Обратите внимание, что только в этой ситуации требуется нотация с использованием точек.
    """author/*""", // Все элементы, являющиеся дочерними для элементов <author>.
    """book/*/last-name""", // Все элементы <last-name>, являющиеся «внуками» элементов <book>.
    """*/*""", // Все элементы-внуки текущего контекста.
    """*[@specialty]""", // Все элементы с атрибутом specialty.
    """@style""", // Атрибут style текущего контекста.
    """price/@exchange""", // Атрибут exchange элементов <price> в текущем контексте.
    """price/@exchange/total""", // Возвращает пустой набор узлов, потому что атрибуты не содержат дочерних элементов. Такое выражение не запрещено грамматикой языка XPath, но, строго говоря, не является допустимым.
    """book[@style]""", // Все элементы <book> с атрибутами style в текущем контексте.
    """book/@style""", // Атрибут style для всех элементов <book> в текущем контексте.
    """@*""", // Все атрибуты контекста текущего элемента.
    """./first-name""", // Все элементы <first-name> текущего узла контекста. Заметьте, что это эквивалентно выражению в следующей строке.
    """first-name""", // Все элементы <first-name> текущего узла контекста.
    """author[1]""", // Первый элемент <author> в текущем узле контекста.
    """author[first-name][3]""", // Третий элемент <author>, имеющий дочерний элемент <first-name>.
    """my:book""", // Элемент <book> из пространства имен my.
    """my:*""", // Все элементы из пространства имен my.
    """@my:*""", // Все атрибуты из пространства имен my (сюда не входят неквалифицированные атрибуты, принадлежащие элементам из пространства имен my).

    """x/y[1]""", // The first <y> child of each <x>. This is equivalent to the expression in the next row.
    """x/y[position() = 1]""", // The first <y> child of each <x>.
//    """(x/y)[1]""", // The first <y> from the entire set of <y> children of <x> elements.
    """x[1]/y[2]""", // The second <y> child of the first <x>.  )

    """book[last()]""", // The last <book> element of the current context node.
    """book/author[last()]""", // The last <author> child of each <book> element of the current context node.
//    """(book/author)[last()]""", // The last <author> element from the entire set of <author> children of <book> elements of the current context node.
    """book[excerpt]""", // All <book> elements that contain at least one <excerpt> element child.
    """book[excerpt]/title""", // All <title> elements that are children of <book> elements that also contain at least one <excerpt> element child.
    """book[excerpt]/author[degree]""", // All <author> elements that contain at least one <degree> element child, and that are children of <book> elements that also contain at least one <excerpt> element.
    """book[author/degree]""", // All <book> elements that contain <author> children that in turn contain at least one <degree> child.
    """author[degree][award]""", // All <author> elements that contain at least one <degree> element child and at least one <award> element child.
    """author[degree and award]""", // All <author> elements that contain at least one <degree> element child and at least one <award> element child.
    """author[(degree or award) and publication]""", // All <author> elements that contain at least one <degree> or <award> and at least one <publication> as the children
    """author[degree and not(publication)]""", // All <author> elements that contain at least one <degree> element child and that contain no <publication> element children.
    """author[not(degree or award) and publication]""", // All <author> elements that contain at least one <publication> element child and contain neither <degree> nor <award> element children.
    """author[last-name = "Bob"]""", // All <author> elements that contain at least one <last-name> element child with the value Bob.
    """author[last-name[1] = "Bob"]""", // All <author> elements where the first <last-name> child element has the value Bob. Note that this is equivalent to the expression in the next row.
    """author[last-name [position()=1]= "Bob"]""", // All <author> elements where the first <last-name> child element has the value Bob.
    """degree[@from != "Harvard"]""", // All <degree> elements where the from attribute is not equal to "Harvard".
    """author[. = "Matthew Bob"]""", // All <author> elements whose value is Matthew Bob.
    """author[last-name = "Bob" and ../price > 50]""", // All <author> elements that contain a <last-name> child element whose value is Bob, and a <price> sibling element whose value is greater than 50.
    """book[position() <= 3]""", // The first three books (1, 2, 3).
    """author[not(last-name = "Bob")]""", // All <author> elements that do no contain <last-name> child elements with the value Bob.
    """author[first-name = "Bob"]""", // All <author> elements that have at least one <first-name> child with the value Bob.
    """author[* = "Bob"]""", // all author elements containing any child element whose value is Bob.
    """author[last-name = "Bob" and first-name = "Joe"]""", // All <author> elements that has a <last-name> child element with the value Bob and a <first-name> child element with the value Joe.
    """price[@intl = "Canada"]""", // All <price> elements in the context node which have an intl attribute equal to "Canada".
    """degree[position() < 3]""", // The first two <degree> elements that are children of the context node.
    """p/text()[2]""", // The second text node in each <p> element in the context node.
    """ancestor::book[1]""", // The nearest <book> ancestor of the context node.
    """ancestor::book[author][1]""", // The nearest <book> ancestor of the context node and this <book> element has an <author> element as its child.
    """ancestor::author[parent::book][1]""" // The nearest <author> ancestor in the current context and this <author> element is a child of a <book> element.
  )
}
