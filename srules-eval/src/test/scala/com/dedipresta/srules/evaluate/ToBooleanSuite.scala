package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToBooleanSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate toBoolean function (int)") {
    assertEquals(
      SRules.parse("toBoolean(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (string)") {
    assertEquals(
      SRules.parse("toBoolean(\"true\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (double)") {
    assertEquals(
      SRules.parse("toBoolean(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (float)") {
    assertEquals(
      SRules.parse("toBoolean(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (long)") {
    assertEquals(
      SRules.parse("toBoolean(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (boolean)") {
    assertEquals(
      SRules.parse("toBoolean(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (boolean false)") {
    assertEquals(
      SRules.parse("toBoolean(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails when argument is a list") {
    assertEquals(
      SRules.parse("toBoolean([1,2,3])").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
