package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class IfSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate if-else expression with 3 arguments (true)") {
    assertEquals(
      SRules.parse("if(true, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (false)") {
    assertEquals(
      SRules.parse("if(false, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (true computed)") {
    assertEquals(
      SRules.parse("if(1 + 1 > 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (false computed)") {
    assertEquals(
      SRules.parse("if(1 + 1 < 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 5 arguments (elseif true)") {
    assertEquals(
      SRules.parse("if(false, 42, 4==4, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 5 arguments (elseif false)") {
    assertEquals(
      SRules.parse("if(false, 42, 4==5, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate if-else expression and evaluate the value") {
    assertEquals(
      SRules.parse("if(true, 1/0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.DivisionByZero("/", List(Expr.RInt(1), Expr.RInt(0)))),
    )
  }

  test("parse and evaluate if-elseif-else expression but does not evaluate the following conditions if the first is true") {
    assertEquals(
      SRules.parse("if(true, 1, 1/0>0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 7 arguments (elseif false)") {
    assertEquals(
      SRules.parse("if(false, 42, false, 1, false, 2, 3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

}
