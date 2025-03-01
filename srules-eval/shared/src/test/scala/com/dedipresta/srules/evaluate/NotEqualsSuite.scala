package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class NotEqualsSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate not equal expression (int)") {
    assertEquals(
      SRules.parse("1!=2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (string)") {
    assertEquals(
      SRules.parse("\"hello\"!=\"world\"").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (double)") {
    assertEquals(
      SRules.parse("1.0!=2.0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (float)") {
    assertEquals(
      SRules.parse("1.0f!=2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (long)") {
    assertEquals(
      SRules.parse("1L!=2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (boolean)") {
    assertEquals(
      SRules.parse("true!=false").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (empty as true)") {
    assertEquals(
      SRules.parse("!=()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (single value as true)") {
    assertEquals(
      SRules.parse("!=(1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (true)") {
    assertEquals(
      SRules.parse("!=(1,2,3,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 4.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

}
