package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToLongSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate toLong function (int)") {
    assertEquals(
      SRules.parse("toLong(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (string)") {
    assertEquals(
      SRules.parse("toLong(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (double)") {
    assertEquals(
      SRules.parse("toLong(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (float)") {
    assertEquals(
      SRules.parse("toLong(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (long)") {
    assertEquals(
      SRules.parse("toLong(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (boolean)") {
    assertEquals(
      SRules.parse("toLong(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate toLong function (boolean false)") {
    assertEquals(
      SRules.parse("toLong(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(0L)),
    )
  }

  test("fails when argument is a list") {
    assertEquals(
      SRules.parse("toLong([1,2,3])").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
