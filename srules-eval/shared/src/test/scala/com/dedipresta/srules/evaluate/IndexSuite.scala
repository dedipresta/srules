package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class IndexSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate index expression") {
    assertEquals(
      SRules.parse("""index()""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("var", List(Expr.RString("__index__")), FailureReason.VariableNotFound("__index__"))),
    )
  }

}
