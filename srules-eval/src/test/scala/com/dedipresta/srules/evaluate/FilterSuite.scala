package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class FilterSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate a filter expression on a list of ints") {
    assertEquals(
      SRules.parse("filter([1,2,3,4], value() % 2 == 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))), // even numbers
    )
  }

  test("parse and evaluate a filter expression on a list of ints (as expr)") {
    assertEquals(
      SRules.parse("filter([1,6-$var1,3,2*2], value() % 2 == 0)").flatMap(evaluator.evaluate(_, Map("var1" -> 4.toExpr))),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))), // even numbers
    )
  }

  test("fails if second argument is not a function") {
    assertEquals(
      SRules.parse("filter([1,2,3,4], 2)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
