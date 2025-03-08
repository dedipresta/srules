package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class LtSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate lt function (ints true)") {
    assertEquals(
      SRules.parse("lt(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (ints false)") {
    assertEquals(
      SRules.parse("lt(1,2,3,2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (ints false with short circuit)") {
    assertEquals(
      SRules.parse("lt(1,3,3,1/0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (double true)") {
    assertEquals(
      SRules.parse("lt(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (double false)") {
    assertEquals(
      SRules.parse("lt(1.0,2.0,3.0,2.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (long true)") {
    assertEquals(
      SRules.parse("lt(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (long false)") {
    assertEquals(
      SRules.parse("lt(1L,2L,3L,2L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (float true)") {
    assertEquals(
      SRules.parse("lt(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (float false)") {
    assertEquals(
      SRules.parse("lt(1.0f,2.0f,3.0f,2.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("returns true by vacuity") {
    assertEquals(
      SRules.parse("lt()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("fails when first argument is not a number") {
    assertEquals(
      SRules.parse("lt(\"a\", 1)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
