package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class EqualsSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("==($x, $y)").flatMap(evaluator.evaluate(_, Map("x" -> 1.toExpr, "y" -> 1.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (int)") {
    assertEquals(
      SRules.parse("1==1").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (string)") {
    assertEquals(
      SRules.parse("\"hello\"==\"hello\"").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (double)") {
    assertEquals(
      SRules.parse("1.0==1.0").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (float)") {
    assertEquals(
      SRules.parse("1.0f==1.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (long)") {
    assertEquals(
      SRules.parse("1L==1L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression (boolean)") {
    assertEquals(
      SRules.parse("true==true").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (empty as true)") {
    assertEquals(
      SRules.parse("==()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (single value as true)") {
    assertEquals(
      SRules.parse("==(1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (true)") {
    assertEquals(
      SRules.parse("==(1,1,1,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 1.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate equal expression with multiple arguments (false)") {
    assertEquals(
      SRules.parse("==(1,1,2,$var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 1.toExpr))),
      Right(Expr.RBoolean(false)),
    )
  }
}
