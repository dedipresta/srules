package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class FloorSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val floor = Floor[Ctx]()
    Map(
      "floor" -> floor,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


  test("parse and evaluate floor function (int)") {
    assertEquals(
      Parser.parser.parseAll("floor(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate floor function (double)") {
    assertEquals(
      Parser.parser.parseAll("floor(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate floor function (float)") {
    assertEquals(
      Parser.parser.parseAll("floor(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate floor function (long)") {
    assertEquals(
      Parser.parser.parseAll("floor(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate floor function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("floor(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.InvalidArgumentType("floor", List(Expr.RBoolean(true)))),
    )
  }

  test("parse and evaluate floor function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("floor(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.InvalidArgumentType("floor", List(Expr.RBoolean(false)))),
    )
  }


}
