package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class MultiplySuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]

  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("(8-6)*1").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate multiplication (ints)") {
    assertEquals(
      SRules.parse("3 * 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(6)),
    )
  }

  test("parse and evaluate multiplication (longs)") {
    assertEquals(
      SRules.parse("3L * 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(6L)),
    )
  }

  test("parse and evaluate multiplication (floats)") {
    assertEquals(
      SRules.parse("3.0f * 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(6.0f)),
    )
  }

  test("parse and evaluate multiplication (doubles)") {
    assertEquals(
      SRules.parse("3.0d * 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between ints and longs") {
    assertEquals(
      SRules.parse("3 * 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(6L)),
    )
  }

  test("parse and evaluate multiplication between ints and floats") {
    assertEquals(
      SRules.parse("3 * 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(6.0f)),
    )
  }

  test("parse and evaluate multiplication between ints and doubles") {
    assertEquals(
      SRules.parse("3 * 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between longs and ints") {
    assertEquals(
      SRules.parse("3L * 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(6L)),
    )
  }

  test("parse and evaluate multiplication between longs and floats") {
    assertEquals(
      SRules.parse("3L * 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(6.0f)),
    )
  }

  test("parse and evaluate multiplication between longs and doubles") {
    assertEquals(
      SRules.parse("3L * 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between floats and ints") {
    assertEquals(
      SRules.parse("3.0f * 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(6.0f)),
    )
  }

  test("parse and evaluate multiplication between floats and longs") {
    assertEquals(
      SRules.parse("3.0f * 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(6.0f)),
    )
  }

  test("parse and evaluate multiplication between floats and doubles") {
    assertEquals(
      SRules.parse("3.0f * 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between doubles and ints") {
    assertEquals(
      SRules.parse("3.0d * 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between doubles and longs") {
    assertEquals(
      SRules.parse("3.0d * 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("parse and evaluate multiplication between doubles and floats") {
    assertEquals(
      SRules.parse("3.0d * 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(6.0d)),
    )
  }

  test("returns 1 when no arguments") {
    assertEquals(
      SRules.parse("*()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("fails when first argument is not a number") {
    assertEquals(
      SRules.parse("*(\"a\", 2)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when second argument is not a number") {
    assertEquals(
      SRules.parse("*(1, \"a\")").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
