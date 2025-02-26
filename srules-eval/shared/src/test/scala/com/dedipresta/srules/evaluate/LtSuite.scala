package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class LtSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val lt = Lt[Ctx]()
    Map(
      "+" -> Add(),
      "<"         -> lt,
      "lt"        -> lt,  // alias
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


  test("parse and evaluate lt function (ints true)") {
    assertEquals(
      Parser.parser.parseAll("lt(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (ints false)") {
    assertEquals(
      Parser.parser.parseAll("lt(1,2,3,2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (ints false with short circuit)") {
    assertEquals(
      Parser.parser.parseAll("lt(1,3,3,1/0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (double true)") {
    assertEquals(
      Parser.parser.parseAll("lt(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (double false)") {
    assertEquals(
      Parser.parser.parseAll("lt(1.0,2.0,3.0,2.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (long true)") {
    assertEquals(
      Parser.parser.parseAll("lt(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (long false)") {
    assertEquals(
      Parser.parser.parseAll("lt(1L,2L,3L,2L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate lt function (float true)") {
    assertEquals(
      Parser.parser.parseAll("lt(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate lt function (float false)") {
    assertEquals(
      Parser.parser.parseAll("lt(1.0f,2.0f,3.0f,2.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }



}
