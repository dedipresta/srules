package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class ToDoubleSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    Map(
      "toDouble" -> ToDouble(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


  test("parse and evaluate toDouble function (int)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (string)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (double)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (float)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (long)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toDouble(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(0.0)),
    )
  }


}
