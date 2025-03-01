package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class AndSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("true && forAll([1,2,3,4], value() > 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate and expression") {
    assertEquals(
      SRules.parse("true&&false").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("and has a short circuit that does not evaluate the second argument") {
    assertEquals(
      SRules.parse("false && (1/0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
