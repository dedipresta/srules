package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class MaxSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate max function (ints)") {
    assertEquals(
      SRules.parse("max(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(4)),
    )
  }

  test("parse and evaluate max function (doubles)") {
    assertEquals(
      SRules.parse("max(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0)),
    )
  }

  test("parse and evaluate max function (floats)") {
    assertEquals(
      SRules.parse("max(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function (longs)") {
    assertEquals(
      SRules.parse("max(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(4L)),
    )
  }

  test("parse and evaluate max function (strings)") {
    assertEquals(
      SRules.parse("max(\"a\",\"b\",\"c\",\"d\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("d")),
    )
  }

  test("parse and evaluate max function between ints and longs") {
    assertEquals(
      SRules.parse("max(1,2L,3,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(4L)),
    )
  }

  test("parse and evaluate max function between ints and floats") {
    assertEquals(
      SRules.parse("max(1,2.0f,3,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function between ints and doubles") {
    assertEquals(
      SRules.parse("max(1,2.0d,3,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("parse and evaluate max function between longs and ints") {
    assertEquals(
      SRules.parse("max(1L,2,3L,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(4L)),
    )
  }

  test("parse and evaluate max function between longs and floats") {

    assertEquals(
      SRules.parse("max(1L,2.0f,3L,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function between longs and doubles") {

    assertEquals(
      SRules.parse("max(1L,2.0d,3L,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("parse and evaluate max function between floats and ints") {
    assertEquals(
      SRules.parse("max(1.0f,2,3.0f,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function between floats and longs") {
    assertEquals(
      SRules.parse("max(1.0f,2L,3.0f,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function between floats and doubles") {
    assertEquals(
      SRules.parse("max(1.0f,2.0d,3.0f,4.0d)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("parse and evaluate max function between doubles and ints") {
    assertEquals(
      SRules.parse("max(1.0d,2,3.0d,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("parse and evaluate max function between doubles and longs") {
    assertEquals(
      SRules.parse("max(1.0d,2L,3.0d,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("parse and evaluate max function between doubles and floats") {
    assertEquals(
      SRules.parse("max(1.0d,2.0f,3.0d,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0d)),
    )
  }

  test("fails when no arguments are provided") {
    assertEquals(
      SRules.parse("max()").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails first argument is not a number or string") {
    assertEquals(
      SRules.parse("""max([], 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
