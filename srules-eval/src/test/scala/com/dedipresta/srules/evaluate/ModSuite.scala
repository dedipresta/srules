package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ModSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate mod function (ints)") {
    assertEquals(
      SRules.parse("3 % 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate mod function (longs)") {
    assertEquals(
      SRules.parse("3L % 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate mod function (floats)") {
    assertEquals(
      SRules.parse("3.0f % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate mod function (doubles)") {
    assertEquals(
      SRules.parse("3.0d % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between ints and longs") {
    assertEquals(
      SRules.parse("3 % 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate mod function between ints and floats") {
    assertEquals(
      SRules.parse("3 % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate mod function between ints and doubles") {
    assertEquals(
      SRules.parse("3 % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between longs and ints") {
    assertEquals(
      SRules.parse("3L % 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate mod function between longs and floats") {

    assertEquals(
      SRules.parse("3L % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate mod function between longs and doubles") {

    assertEquals(
      SRules.parse("3L % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between floats and ints") {

    assertEquals(
      SRules.parse("3.0f % 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate mod function between floats and longs") {

    assertEquals(
      SRules.parse("3.0f % 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate mod function between floats and doubles") {

    assertEquals(
      SRules.parse("3.0f % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between doubles and ints") {

    assertEquals(
      SRules.parse("3.0d % 2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between doubles and longs") {

    assertEquals(
      SRules.parse("3.0d % 2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate mod function between doubles and floats") {

    assertEquals(
      SRules.parse("3.0d % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0d)),
    )
  }

  test("parse and evaluate fails when mod 0 (int % int)") {
    assertEquals(
      SRules.parse("3 % 0").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when mod 0 (long % long)") {
    assertEquals(
      SRules.parse("3L % 0L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when mod 0 (long % int)") {
    assertEquals(
      SRules.parse("3L % 0").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when mod 0 (int % long)") {
    assertEquals(
      SRules.parse("3 % 0L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when first argument is not a number") {
    assertEquals(
      SRules.parse("mod(\"a\", 2)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when second argument is not a number") {
    assertEquals(
      SRules.parse("mod(3, \"a\")").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (int % float)") {
    assertEquals(
      SRules.parse("3 % 0f").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (int % double)") {
    assertEquals(
      SRules.parse("3 % 0d").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (long % float)") {
    assertEquals(
      SRules.parse("3L % 0f").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (long % double)") {
    assertEquals(
      SRules.parse("3L % 0d").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (float % int)") {
    assertEquals(
      SRules.parse("3f % 0").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (float % long)") {
    assertEquals(
      SRules.parse("3f % 0L").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (double % int)") {
    assertEquals(
      SRules.parse("3d % 0").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (double % long)") {
    assertEquals(
      SRules.parse("3d % 0L").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate fails when no arguments are provided") {
    assertEquals(
      SRules.parse("mod()").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when only one argument is provided") {
    assertEquals(
      SRules.parse("mod(1)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails when more than two arguments are provided") {
    assertEquals(
      SRules.parse("mod(1,2,3)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
