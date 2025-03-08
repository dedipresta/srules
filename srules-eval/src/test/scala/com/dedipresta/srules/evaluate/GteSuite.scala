package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class GteSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate gte function (ints true)") {
    assertEquals(
      SRules.parse("gte(4,3,2,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }
  test("parse and evaluate gte function (longs true)") {
    assertEquals(
      SRules.parse("gte(4L,3L,2L,1L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gte function (floats true)") {
    assertEquals(
      SRules.parse("gte(4.0f,3.0f,2.0f,1.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gte function (doubles true)") {
    assertEquals(
      SRules.parse("gte(4.0d,3.0d,2.0d,1.0d)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gte function (ints false)") {
    assertEquals(
      SRules.parse("gte(4,3,2,3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("returns true by vacuity") {
    assertEquals(
      SRules.parse("gte()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("fails if first argument is not a number") {
    assertEquals(
      SRules.parse("""gte("a", 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
