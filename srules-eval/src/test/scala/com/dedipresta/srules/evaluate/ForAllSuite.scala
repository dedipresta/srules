package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ForAllSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate a forAll expression (true)") {
    assertEquals(
      SRules.parse("forAll([1, 2, 3], value()>0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate a forAll expression (false)") {
    assertEquals(
      SRules.parse("forAll([1, 2, 3], value()<3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("has a short-circuit evaluation") {
    assertEquals(
      SRules.parse("forAll([1, 2, 3/0], value()>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails if second argument is not a function") {
    assertEquals(
      SRules.parse("forAll([1, 2, 3], 2)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
