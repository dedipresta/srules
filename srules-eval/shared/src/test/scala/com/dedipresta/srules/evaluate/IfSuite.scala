package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class IfSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val and  = And[Ctx]()
    Map(
      "if"  -> If[Ctx](),
      "and" -> and,
      "&&"  -> and,
      "+"   -> Add[Ctx](),
      ">"   -> Gt(),
      "<"   -> Lt(),
      "/"   -> Divide[Ctx](),
      "=="  -> Equal(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate if-else expression with 3 arguments (true)") {
    assertEquals(
      Parser.parser.parseAll("if(true, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (false)") {
    assertEquals(
      Parser.parser.parseAll("if(false, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (true computed)") {
    assertEquals(
      Parser.parser.parseAll("if(1 + 1 > 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate if-else expression with 3 arguments (false computed)") {
    assertEquals(
      Parser.parser.parseAll("if(1 + 1 < 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 5 arguments (elseif true)") {
    assertEquals(
      Parser.parser.parseAll("if(false, 42, 4==4, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 5 arguments (elseif false)") {
    assertEquals(
      Parser.parser.parseAll("if(false, 42, 4==5, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate if-else expression but does not evaluate the value") {
    assertEquals(
      Parser.parser.parseAll("if(true, 1/0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFunction("/", List(Expr.RInt(1), Expr.RInt(0)))),
    )
  }

  test("parse and evaluate if-elseif-else expression but does not evaluate the following conditions if the first is true") {
    assertEquals(
      Parser.parser.parseAll("if(true, 1, 1/0>0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate if-elseif-else expression with 7 arguments (elseif false)") {
    assertEquals(
      Parser.parser.parseAll("if(false, 42, false, 1, false, 2, 3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

}
