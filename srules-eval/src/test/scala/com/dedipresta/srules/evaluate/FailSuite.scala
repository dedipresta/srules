package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class FailSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    val f = Expr.RFunction("var", "message".toExpr)
    assertEquals(
      Parser.parser
        .parseAll("fail($message)")
        .flatMap(evaluator.evaluate(_, Map("message" -> "error message".toExpr))),
      Left(EvaluationError.OperationFailure("fail", List(f), FailureReason.Message("error message"))),
    )
  }

  test("parse and evaluate fail function (no message)") {
    assertEquals(
      SRules.parse("fail()").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("fail", Nil, FailureReason.Message("Fail operator called"))),
    )
  }

  test("parse and evaluate fail function (with message)") {
    assertEquals(
      SRules.parse("fail(\"error message\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(
        EvaluationError.OperationFailure(
          "fail",
          List(Expr.RString("error message")),
          FailureReason.Message("error message"),
        ),
      ),
    )
  }

  test("fails if second argument is not a string") {
    assertEquals(
      SRules.parse("fail(42, 42)").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
