package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class FailSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate fail function (no message)") {
    assertEquals(
      Parser.parser.parseAll("fail()").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("fail", Nil, FailureReason.Message("Fail operator called"))),
    )
  }

  test("parse and evaluate fail function (with message)") {
    assertEquals(
      Parser.parser.parseAll("fail(\"error message\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(
        EvaluationError.OperationFailure(
          "fail",
          List(Expr.RString("error message")),
          FailureReason.Message("error message"),
        ),
      ),
    )
  }

}
