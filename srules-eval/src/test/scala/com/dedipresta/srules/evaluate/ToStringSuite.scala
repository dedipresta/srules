package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToStringSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate toString function (int)") {
    assertEquals(
      SRules.parse("toString(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42")),
    )
  }

  test("parse and evaluate toString function (string)") {
    assertEquals(
      SRules.parse("toString(\"hello\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("\"hello\"")),
    )
  }

  test("parse and evaluate toString function (long)") {
    assertEquals(
      SRules.parse("toString(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42")),
    )
  }

  test("parse and evaluate toString function (boolean)") {
    assertEquals(
      SRules.parse("toString(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("true")),
    )
  }

  test("parse and evaluate toString function (boolean false)") {
    assertEquals(
      SRules.parse("toString(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("false")),
    )
  }

  test("parse and evaluate toString function (double)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.0d")),
    )
  }

  test("parse and evaluate toString function (double with decimal)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.5)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.5d")),
    )
  }

  test("parse and evaluate toString function (float)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.0f")),
    )
  }

  test("parse and evaluate toString function (float with decimal)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.5f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.5f")),
    )
  }

}
