package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class GtSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate gt function (ints true)") {
    assertEquals(
      SRules.parse("gt(4,3,2,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gt function (inner evaluation required)") {
    assertEquals(
      SRules.parse("gt(4,3,1+1,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gt function (ints false)") {
    assertEquals(
      SRules.parse("gt(4,3,2,3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
