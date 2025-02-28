package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class AbsSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate abs function (int positive)") {
    assertEquals(
      Parser.parser.parseAll("abs(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate abs function (int negative)") {
    assertEquals(
      Parser.parser.parseAll("abs(-42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate abs function (float positive)") {
    assertEquals(
      Parser.parser.parseAll("abs(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0)),
    )
  }

  test("parse and evaluate abs function (float negative)") {
    assertEquals(
      Parser.parser.parseAll("abs(-42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0)),
    )
  }

  test("parse and evaluate abs function (double positive)") {
    assertEquals(
      Parser.parser.parseAll("abs(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate abs function (double negative)") {
    assertEquals(
      Parser.parser.parseAll("abs(-42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate abs function (long positive)") {
    assertEquals(
      Parser.parser.parseAll("abs(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate abs function (long negative)") {
    assertEquals(
      Parser.parser.parseAll("abs(-42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

}
