package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class MinSameTypeSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val operators: Map[String, Operator[ErrorOr, Map[String, Expr], EvaluationError]] =
    DefaultOperators.all[ErrorOr, Map[String, Expr]] ++
      Map(
        "min" -> Min[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
      )

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(operators)

  test("parse and evaluate min function (ints)") {
    assertEquals(
      SRules.parse("min(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate min function (doubles)") {
    assertEquals(
      SRules.parse("min(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0)),
    )
  }

  test("parse and evaluate min function (floats)") {
    assertEquals(
      SRules.parse("min(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate min function (longs)") {
    assertEquals(
      SRules.parse("min(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate min function (strings)") {
    assertEquals(
      SRules.parse("min(\"a\",\"b\",\"c\",\"d\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("a")),
    )
  }

  test("parse and evaluate fails for min function between ints and longs") {
    assertEquals(
      SRules.parse("min(1,2L,3,4L)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between ints and floats") {
    assertEquals(
      SRules.parse("min(1,2.0f,3,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between ints and doubles") {
    assertEquals(
      SRules.parse("min(1,2.0d,3,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between longs and ints") {
    assertEquals(
      SRules.parse("min(1L,2,3L,4)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between longs and floats") {
    assertEquals(
      SRules.parse("min(1L,2.0f,3L,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between longs and doubles") {
    assertEquals(
      SRules.parse("min(1L,2.0d,3L,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between floats and ints") {
    assertEquals(
      SRules.parse("min(1.0f,2,3.0f,4)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between floats and longs") {
    assertEquals(
      SRules.parse("min(1.0f,2L,3.0f,4L)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between floats and doubles") {
    assertEquals(
      SRules.parse("min(1.0f,2.0d,3.0f,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between doubles and ints") {
    assertEquals(
      SRules.parse("min(1.0d,2,3.0d,4)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between doubles and longs") {
    assertEquals(
      SRules.parse("min(1.0d,2L,3.0d,4L)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate fails for min function between doubles and floats") {
    assertEquals(
      SRules.parse("min(1.0d,2.0f,3.0d,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when no arguments are provided") {
    assertEquals(
      SRules.parse("min()").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails first argument is not a number or string") {
    assertEquals(
      SRules.parse("""min(["a"], 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
