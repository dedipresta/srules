package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class FindSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate a find expression (found)") {
    assertEquals(
      SRules.parse("find([1, 2, 3, 4], value()>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate a find expression (not found)") {
    assertEquals(
      SRules.parse("find([1, 2, 3, 4], value()>4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("has a short-circuit evaluation") {
    assertEquals(
      SRules.parse("find([1, 2, 3, 4/0], value()>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

}
