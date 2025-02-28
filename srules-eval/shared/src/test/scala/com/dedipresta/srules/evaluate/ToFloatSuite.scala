package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToFloatSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate toFloat function (int)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (string)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (double)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (float)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (long)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate toFloat function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(1.0f)),
    )
  }

  test("parse and evaluate toFloat function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toFloat(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(0.0f)),
    )
  }

}
