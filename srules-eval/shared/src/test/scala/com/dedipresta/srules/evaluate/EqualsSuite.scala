package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class EqualsSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate equal expression (int)") {
    assertEquals(
      Parser.parser.parseAll("1==1").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (string)") {
    assertEquals(
      Parser.parser.parseAll("\"hello\"==\"hello\"").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (double)") {
    assertEquals(
      Parser.parser.parseAll("1.0==1.0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (float)") {
    assertEquals(
      Parser.parser.parseAll("1.0f==1.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (long)") {
    assertEquals(
      Parser.parser.parseAll("1L==1L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (boolean)") {
    assertEquals(
      Parser.parser.parseAll("true==true").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (empty as true)") {
    assertEquals(
      Parser.parser.parseAll("==()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (single value as true)") {
    assertEquals(
      Parser.parser.parseAll("==(1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (true)") {
    assertEquals(
      Parser.parser.parseAll("==(1,1,1,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 1))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (false)") {
    assertEquals(
      Parser.parser.parseAll("==(1,1,2,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 1))),
      Right(Expr.RBoolean(false)),
    )
  }
}
