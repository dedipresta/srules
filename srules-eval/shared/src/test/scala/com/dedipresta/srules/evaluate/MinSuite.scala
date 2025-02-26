package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class MinSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val min = Min[Ctx]()
    Map(
      "min" -> min,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


  test("parse and evaluate min function (ints)") {
    assertEquals(
      Parser.parser.parseAll("min(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate min function (doubles)") {
    assertEquals(
      Parser.parser.parseAll("min(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0)),
    )
  }

  test("parse and evaluate min function (floats)") {
    assertEquals(
      Parser.parser.parseAll("min(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate min function (longs)") {
    assertEquals(
      Parser.parser.parseAll("min(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(1L)),
    )
  }

  test("parse and evaluate min function (strings)") {
    assertEquals(
      Parser.parser.parseAll("min(\"a\",\"b\",\"c\",\"d\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("a")),
    )
  }

}
