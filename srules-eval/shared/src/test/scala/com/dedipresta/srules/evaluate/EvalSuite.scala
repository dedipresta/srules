package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class EvalSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("eval($x)").flatMap(evaluator.evaluate(_, Map("x" -> Expr.RFunction("+", List(Expr.RInt(1), Expr.RInt(1)))))),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate eval expression") {
    assertEquals(
      SRules.parse("eval(1 + 1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("eval allows to force evaluation of returned value from a if-else expression") {
    assertEquals(
      SRules.parse("eval(if(true, 1 + 1, 5))").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("eval allows to force evaluation of a list sub-expressions") {
    assertEquals(
      SRules.parse("eval([1 + 1, 2 + 2])").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))),
    )
  }

  test("eval allows to force evaluation of a list sub-expressions (deep)") {
    assertEquals(
      SRules.parse("eval([1 + 1, [2 + 2, 3 + 3]])").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RList(List(Expr.RInt(4), Expr.RInt(6)))))),
    )
  }
}
