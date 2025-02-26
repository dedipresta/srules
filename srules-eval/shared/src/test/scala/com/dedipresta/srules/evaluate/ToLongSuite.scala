package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToLongSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    Map(
      "toLong" -> ToLong(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate toLong function (int)") {
    assertEquals(
      Parser.parser.parseAll("toLong(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (string)") {
    assertEquals(
      Parser.parser.parseAll("toLong(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (double)") {
    assertEquals(
      Parser.parser.parseAll("toLong(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (float)") {
    assertEquals(
      Parser.parser.parseAll("toLong(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (long)") {
    assertEquals(
      Parser.parser.parseAll("toLong(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate toLong function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toLong(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate toLong function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toLong(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(0L)),
    )
  }

}
