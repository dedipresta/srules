package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToIntSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate toInt function (int)") {
    assertEquals(
      Parser.parser.parseAll("toInt(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (string)") {
    assertEquals(
      Parser.parser.parseAll("toInt(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (double)") {
    assertEquals(
      Parser.parser.parseAll("toInt(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (float)") {
    assertEquals(
      Parser.parser.parseAll("toInt(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (long)") {
    assertEquals(
      Parser.parser.parseAll("toInt(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (boolean)") {
    assertEquals(
      Parser.parser.parseAll("toInt(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate toInt function (boolean false)") {
    assertEquals(
      Parser.parser.parseAll("toInt(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

}
