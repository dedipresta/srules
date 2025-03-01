package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToDoubleSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate toDouble function (int)") {
    assertEquals(
      SRules.parse("toDouble(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (string)") {
    assertEquals(
      SRules.parse("toDouble(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (double)") {
    assertEquals(
      SRules.parse("toDouble(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (float)") {
    assertEquals(
      SRules.parse("toDouble(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (long)") {
    assertEquals(
      SRules.parse("toDouble(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean)") {
    assertEquals(
      SRules.parse("toDouble(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean false)") {
    assertEquals(
      SRules.parse("toDouble(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(0.0)),
    )
  }

}
