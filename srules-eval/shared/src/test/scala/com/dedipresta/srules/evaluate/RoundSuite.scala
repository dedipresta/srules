package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class RoundSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate round function (int)") {
    assertEquals(
      SRules.parse("round(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate round function (double)") {
    assertEquals(
      SRules.parse("round(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate round function (double)") {
    assertEquals(
      SRules.parse("round(42.4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate round function (double)") {
    assertEquals(
      SRules.parse("round(42.5)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(43.0)),
    )
  }

  test("parse and evaluate round function (float)") {
    assertEquals(
      SRules.parse("round(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate round function (long)") {
    assertEquals(
      SRules.parse("round(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate round function (boolean)") {
    assertEquals(
      SRules.parse("round(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("round", List(Expr.RBoolean(true)), FailureReason.InvalidArgumentType("Numeric", Expr.RBoolean(true)))),
    )
  }

}
