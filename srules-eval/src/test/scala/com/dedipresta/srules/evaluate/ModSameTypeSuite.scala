package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ModSameTypeSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]

  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val operators: Map[String, Operator[ErrorOr, Map[String, Expr], EvaluationError]] =
    DefaultOperators.all[ErrorOr, Map[String, Expr]] ++
      Map(
        "%"   -> Mod[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
        "mod" -> Mod[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
      )

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(operators)

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

  test("parse and evaluate mod function fails between ints and longs") {
    assertEquals(
      SRules.parse("3 % 2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between ints and floats") {
    assertEquals(
      SRules.parse("3 % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between ints and doubles") {
    assertEquals(
      SRules.parse("3 % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between longs and ints") {
    assertEquals(
      SRules.parse("3L % 2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between longs and floats") {
    assertEquals(
      SRules.parse("3L % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between longs and doubles") {
    assertEquals(
      SRules.parse("3L % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between floats and ints") {
    assertEquals(
      SRules.parse("3.0f % 2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between floats and longs") {
    assertEquals(
      SRules.parse("3.0f % 2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between floats and doubles") {
    assertEquals(
      SRules.parse("3.0f % 2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between doubles and ints") {
    assertEquals(
      SRules.parse("3.0d % 2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between doubles and longs") {
    assertEquals(
      SRules.parse("3.0d % 2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate mod function fails between doubles and floats") {
    assertEquals(
      SRules.parse("3.0d % 2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
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

  test("parse and evaluate succeeds when mod 0 (float % float)") {
    assertEquals(
      SRules.parse("3.0f % 0.0f").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
      true,
    )
  }

  test("parse and evaluate succeeds when mod 0 (double % double)") {
    assertEquals(
      SRules.parse("3.0d % 0.0d").flatMap(evaluator.evaluate(_, Map.empty)).isRight,
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
