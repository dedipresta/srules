package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class CeilSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate ceil function (int)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate ceil function (double)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate ceil function (double)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42.4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(43.0)),
    )
  }

  test("parse and evaluate ceil function (float)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate ceil function (float)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42.4f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(43.0f)),
    )
  }

  test("parse and evaluate ceil function (long)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate ceil function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("ceil(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("ceil", List(Expr.RBoolean(true)), FailureReason.InvalidArgumentType("Numeric", Expr.RBoolean(true)))),
    )
  }

}
