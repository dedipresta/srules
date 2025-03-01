package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToIntSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate toInt function (int)") {
    assertEquals(
      SRules.parse("toInt(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (string)") {
    assertEquals(
      SRules.parse("toInt(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (double)") {
    assertEquals(
      SRules.parse("toInt(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (float)") {
    assertEquals(
      SRules.parse("toInt(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (long)") {
    assertEquals(
      SRules.parse("toInt(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toInt function (boolean)") {
    assertEquals(
      SRules.parse("toInt(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate toInt function (boolean false)") {
    assertEquals(
      SRules.parse("toInt(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

}
