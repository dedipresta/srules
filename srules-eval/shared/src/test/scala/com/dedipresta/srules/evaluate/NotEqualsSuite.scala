package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class NotEqualsSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate not equal expression (int)") {
    assertEquals(
      Parser.parser.parseAll("1!=2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (string)") {
    assertEquals(
      Parser.parser.parseAll("\"hello\"!=\"world\"").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (double)") {
    assertEquals(
      Parser.parser.parseAll("1.0!=2.0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (float)") {
    assertEquals(
      Parser.parser.parseAll("1.0f!=2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (long)") {
    assertEquals(
      Parser.parser.parseAll("1L!=2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression (boolean)") {
    assertEquals(
      Parser.parser.parseAll("true!=false").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (empty as true)") {
    assertEquals(
      Parser.parser.parseAll("!=()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (single value as true)") {
    assertEquals(
      Parser.parser.parseAll("!=(1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate not equal expression with multiple arguments (true)") {
    assertEquals(
      Parser.parser.parseAll("!=(1,2,3,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 4))),
      Right(Expr.RBoolean(true)),
    )
  }

}
