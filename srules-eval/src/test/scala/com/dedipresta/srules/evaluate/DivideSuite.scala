package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class DivideSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

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

  test("parse and evaluate division between int and long") {
    assertEquals(
      SRules.parse("6/3L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(2L)),
    )
  }

  test("parse and evaluate division between int and float") {
    assertEquals(
      SRules.parse("6/3.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(2.0f)),
    )
  }

  test("parse and evaluate division between int and double") {
    assertEquals(
      SRules.parse("6/3.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("parse and evaluate division between long and int") {
    assertEquals(
      SRules.parse("6L/3").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(2L)),
    )
  }

  test("parse and evaluate division between long and float") {
    assertEquals(
      SRules.parse("6L/3.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(2.0f)),
    )
  }

  test("parse and evaluate division between long and double") {
    assertEquals(
      SRules.parse("6L/3.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("parse and evaluate division between float and double") {
    assertEquals(
      SRules.parse("6.0f/3.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("parse and evaluate division between float and int") {
    assertEquals(
      SRules.parse("6.0f/3").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(2.0f)),
    )
  }

  test("parse and evaluate division between float and long") {
    assertEquals(
      SRules.parse("6.0f/3L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(2.0f)),
    )
  }

  test("parse and evaluate division between double and float") {
    assertEquals(
      SRules.parse("6.0d/3.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("parse and evaluate division between double and int") {
    assertEquals(
      SRules.parse("6.0d/3").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("parse and evaluate division between double and long") {
    assertEquals(
      SRules.parse("6.0d/3L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(2.0d)),
    )
  }

  test("allow division by zero when result is infinity (1d/0)") {
    assertEquals(
      SRules.parse("1d/0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(Double.PositiveInfinity)),
    )
  }

  test("allow division by zero when result is infinity (1f/0)") {
    assertEquals(
      SRules.parse("1f/0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(Float.PositiveInfinity)),
    )
  }

  test("allow division by zero when result is infinity (1/0d)") {
    assertEquals(
      SRules.parse("1/0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(Double.PositiveInfinity)),
    )
  }

  test("allow division by zero when result is infinity (1/0f)") {
    assertEquals(
      SRules.parse("1/0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(Float.PositiveInfinity)),
    )
  }
  test("allow division by zero when result is infinity (1/0d)") {
    assertEquals(
      SRules.parse("1/0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(Double.PositiveInfinity)),
    )
  }

}
