package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class SizeSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate size function on string") {
    assertEquals(
      Parser.parser.parseAll("""size("hello")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(5)),
    )
  }

  test("parse and evaluate size function on list") {
    assertEquals(
      Parser.parser.parseAll("""size(["a","b", "c"])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("return an error otherwise") {
    assertEquals(
      Parser.parser.parseAll("""size(42)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("size", List(Expr.RInt(42)), FailureReason.InvalidArgumentType("List or String", Expr.RInt(42)))),
    )
  }

}
