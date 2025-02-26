package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class ToBooleanSuite extends FunSuite {

  type Ctx = Map[Boolean, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    Map(
      "toBoolean" -> ToBoolean[Ctx](),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)
  
  test("parse and evaluate toBoolean function (int)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (string)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(\"true\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (double)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (float)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (long)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate toBoolean function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toBoolean(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
