package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class PowSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate pow function on ints (4^2)") {
    assertEquals(
      SRules.parse("pow(4, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(16.0)),
    )
  }

  test("parse and evaluate pow function on doubles (4.0^2.0)") {
    assertEquals(
      SRules.parse("pow(4.0, 2.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(16.0)),
    )
  }

}
