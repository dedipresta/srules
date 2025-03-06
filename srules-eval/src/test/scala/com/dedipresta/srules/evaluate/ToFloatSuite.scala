package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToFloatSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate toFloat function (int)") {
    assertEquals(
      SRules.parse("toFloat(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (string)") {
    assertEquals(
      SRules.parse("toFloat(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (double)") {
    assertEquals(
      SRules.parse("toFloat(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (float)") {
    assertEquals(
      SRules.parse("toFloat(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (long)") {
    assertEquals(
      SRules.parse("toFloat(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (boolean)") {
    assertEquals(
      SRules.parse("toFloat(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate toFloat function (boolean false)") {
    assertEquals(
      SRules.parse("toFloat(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(0.0f)),
    )
  }

  test("fails when argument is a list") {
    assertEquals(
      SRules.parse("toFloat([1,2,3])").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
