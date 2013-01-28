package senia.scala_xpath.parser

import org.scalatest.FunSpec
import scala.language.implicitConversions
import org.scalatest.matchers.{ShouldMatchers, MatchResult, Matcher}

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
    """@my:*""" // Все атрибуты из пространства имен my (сюда не входят неквалифицированные атрибуты, принадлежащие элементам из пространства имен my).
  )
}

class ParserSpec extends FunSpec with ShouldMatchers {
  val parser = new XPathParsers

  def beSuccessful[T](s: String) = new Matcher[parser.ParseResult[T]] {
      def apply(r: parser.ParseResult[T]) =
        MatchResult(
          r.successful,
          s"""Parse result of "$s" was not successful: $r""",
          s"""Parse result of "$s" was successful: $r"""
        )
    }

  describe("A Parser") {
  
    for (example <- XPathExamples.examples) {
      it(s"""should parse $example""") {
        parser(example) should beSuccessful(example)
      }
	}
  }
}