package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ValueSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate named expression") {
    assertEquals(
      SRules.parse("""value()""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("var", List(Expr.RString("__value__")), FailureReason.VariableNotFound("__value__"))),
    )
  }

}
