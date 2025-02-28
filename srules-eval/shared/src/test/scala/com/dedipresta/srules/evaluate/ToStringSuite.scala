package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToStringSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate toString function (int)") {
    assertEquals(
      Parser.parser.parseAll("toString(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42")),
    )
  }

  test("parse and evaluate toString function (string)") {
    assertEquals(
      Parser.parser.parseAll("toString(\"hello\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("\"hello\"")),
    )
  }

  test("parse and evaluate toString function (long)") {
    assertEquals(
      Parser.parser.parseAll("toString(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42")),
    )
  }

  test("parse and evaluate toString function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toString(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("true")),
    )
  }

  test("parse and evaluate toString function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toString(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("false")),
    )
  }

}
