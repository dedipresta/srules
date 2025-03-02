package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ReduceSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate a reduce expression on an array of ints") {
    assertEquals(
      SRules.parse("reduce([1,2,3,4], value()+acc())").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(10)),
    )
  }

  test("parse and evaluate a reduce expression on an array of ints (with sub expressions)") {
    assertEquals(
      SRules.parse("reduce([1,2,3+1,4], value()+acc())").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(11)),
    )
  }

  test("parse and evaluate a reduce expression on an array of ints (one int only)") {
    assertEquals(
      SRules.parse("reduce([10], value()+acc())").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(10)),
    )
  }

  test("reduce should return an error when the array is empty") {
    assertEquals(
      SRules.parse("reduce([], value()+acc())").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(
        EvaluationError.OperationFailure(
          "reduce",
          List(
            Expr.RList(Nil),
            Expr.RFunction("+", List(Expr.RFunction("value"), Expr.RFunction("acc"))),
          ),
          FailureReason.InvalidArgumentValue("Non-empty list", Expr.RList(Nil)),
        ),
      ),
    )
  }

}
