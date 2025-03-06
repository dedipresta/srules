package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class DivideSameTypeSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val operators: Map[String, Operator[ErrorOr, Map[String, Expr], EvaluationError]] =
    DefaultOperators.all[ErrorOr, Map[String, Expr]] ++
      Map(
        "/" -> Divide[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
      )

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(operators)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("(6+9)/5").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate division between ints") {
    assertEquals(
      SRules.parse("6/3").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate division between longs") {
    assertEquals(
      SRules.parse("6L/3L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(2L)),
    )
  }

  test("parse and evaluate division between floats") {
    assertEquals(
      SRules.parse("6.0f/3.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(2.0f)),
    )
  }

  test("parse and evaluate division between doubles") {
    assertEquals(
      SRules.parse("6.0d/3.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("fails when arguments are not numbers") {
    assertEquals(
      SRules.parse("""6/"3"""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division by zero (ints)") {
    assertEquals(
      SRules.parse("6/0").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division by zero (longs)") {
    assertEquals(
      SRules.parse("6L/0L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between ints and longs") {
    assertEquals(
      SRules.parse("6/3L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between ints and floats") {
    assertEquals(
      SRules.parse("6/3.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between ints and doubles") {
    assertEquals(
      SRules.parse("6/3.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between longs and floats") {
    assertEquals(
      SRules.parse("6L/3.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between longs and doubles") {
    assertEquals(
      SRules.parse("6L/3.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between floats and doubles") {
    assertEquals(
      SRules.parse("6.0f/3.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between floats and ints") {
    assertEquals(
      SRules.parse("6.0f/3").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between floats and longs") {
    assertEquals(
      SRules.parse("6.0f/3L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between doubles and floats") {
    assertEquals(
      SRules.parse("6.0d/3.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when division between doubles and longs") {
    assertEquals(
      SRules.parse("6.0d/3L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
