package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class LazyIfSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate lazyIf-else expression with 3 arguments (true)") {
    assertEquals(
      SRules.parse("lazyIf(true, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate lazyIf-else expression with 3 arguments (false)") {
    assertEquals(
      SRules.parse("lazyIf(false, 42, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

  test("parse and evaluate lazyIf-else expression with 3 arguments (true computed)") {
    assertEquals(
      SRules.parse("lazyIf(1 + 1 > 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate lazyIf-else expression with 3 arguments (false computed)") {
    assertEquals(
      SRules.parse("lazyIf(1 + 1 < 0, 42, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate lazyIf-elseif-else expression with 5 arguments (elseif true)") {
    assertEquals(
      SRules.parse("lazyIf(false, 42, 4==4, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate lazyIf-elseif-else expression with 5 arguments (elseif false)") {
    assertEquals(
      SRules.parse("lazyIf(false, 42, 4==5, 1, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate lazyIf-else expression but does not evaluate the value") {
    assertEquals(
      SRules.parse("lazyIf(true, 1/0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFunction("/", List(Expr.RInt(1), Expr.RInt(0)))),
    )
  }

  test("parse and evaluate lazyIf-elseif-else expression but does not evaluate the following conditions if the first is true") {
    assertEquals(
      SRules.parse("lazyIf(true, 1, 1/0>0, 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("parse and evaluate lazyIf-elseif-else expression with 7 arguments (elseif false)") {
    assertEquals(
      SRules.parse("lazyIf(false, 42, false, 1, false, 2, 3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

}
