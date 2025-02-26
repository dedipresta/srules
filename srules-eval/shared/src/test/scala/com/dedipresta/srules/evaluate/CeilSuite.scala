package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class CeilSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val ceil = Ceil[Ctx]()
    Map(
      "ceil" -> ceil,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


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

  test("parse and evaluate ceil function (float)") {
    assertEquals(
      Parser.parser.parseAll("ceil(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
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
      Left(EvaluationError.InvalidArgumentType("ceil", List(Expr.RBoolean(true)))),
    )
  }

  test("parse and evaluate ceil function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("ceil(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.InvalidArgumentType("ceil", List(Expr.RBoolean(false)))),
    )
  }


}
