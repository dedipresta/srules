package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class SubtractSameTypeSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]

  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val operators: Map[String, Operator[ErrorOr, Map[String, Expr], EvaluationError]] =
    DefaultOperators.all[ErrorOr, Map[String, Expr]] ++
      Map(
        "-"   -> Subtract[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
        "sub" -> Subtract[ErrorOr, Map[String, Expr]](sameTypeOnly = true),
      )

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(operators)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("(8+6)-1").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(13)),
    )
  }

  test("parse and evaluate subtraction") {
    assertEquals(
      SRules.parse("1-2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("parse and evaluate subtraction between ints") {
    assertEquals(
      SRules.parse("1-2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("parse and evaluate subtraction between longs") {
    assertEquals(
      SRules.parse("1L-2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(-1L)),
    )
  }

  test("parse and evaluate subtraction between floats") {
    assertEquals(
      SRules.parse("1.0f-2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(-1.0f)),
    )
  }

  test("parse and evaluate subtraction between doubles") {
    assertEquals(
      SRules.parse("1.0d-2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(-1.0d)),
    )
  }

  test("parse and evaluate subtraction with variables") {
    assertEquals(
      SRules.parse("$var1-2").flatMap(evaluator.evaluate(_, Map("var1" -> 1.toExpr))),
      Right(Expr.RInt(-1)),
    )
  }

  test("parse and evaluate subtraction fails between int and long") {
    assertEquals(
      SRules.parse("1-2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between int and float") {
    assertEquals(
      SRules.parse("1-2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between int and double") {
    assertEquals(
      SRules.parse("1-2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between long and int") {
    assertEquals(
      SRules.parse("1L-2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between long and float") {
    assertEquals(
      SRules.parse("1L-2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between long and double") {
    assertEquals(
      SRules.parse("1L-2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between float and int") {
    assertEquals(
      SRules.parse("1.0f-2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between float and long") {
    assertEquals(
      SRules.parse("1.0f-2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between float and double") {
    assertEquals(
      SRules.parse("1.0f-2.0d").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between double and int") {
    assertEquals(
      SRules.parse("1.0d-2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between double and long") {
    assertEquals(
      SRules.parse("1.0d-2L").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between double and float") {
    assertEquals(
      SRules.parse("1.0d-2.0f").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("parse and evaluate subtraction fails between string and int") {
    assertEquals(
      SRules.parse("\"1\"-2").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
