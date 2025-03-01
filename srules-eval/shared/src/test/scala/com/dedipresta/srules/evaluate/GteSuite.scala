package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class GteSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate gte function (ints true)") {
    assertEquals(
      SRules.parse("gte(4,3,2,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gte function (ints false)") {
    assertEquals(
      SRules.parse("gte(4,3,2,3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
