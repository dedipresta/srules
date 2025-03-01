package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class SizeSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate size function on string") {
    assertEquals(
      SRules.parse("""size("hello")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(5)),
    )
  }

  test("parse and evaluate size function on list") {
    assertEquals(
      SRules.parse("""size(["a","b", "c"])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("return an error otherwise") {
    assertEquals(
      SRules.parse("""size(42)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("size", List(Expr.RInt(42)), FailureReason.InvalidArgumentType("List or String", Expr.RInt(42)))),
    )
  }

}
