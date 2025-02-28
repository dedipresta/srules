package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ReduceSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate a reduce expression on an array of ints") {
    assertEquals(
      Parser.parser.parseAll("reduce([1,2,3,4], ${__value__} + ${__acc__})").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(10)),
    )
  }

  test("parse and evaluate a reduce expression on an array of ints (with sub expressions)") {
    assertEquals(
      Parser.parser.parseAll("reduce([1,2,3+1,4], ${__value__} + ${__acc__})").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(11)),
    )
  }

  test("parse and evaluate a reduce expression on an array of ints (one int only)") {
    assertEquals(
      Parser.parser.parseAll("reduce([10], ${__value__} + ${__acc__})").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(10)),
    )
  }

  test("reduce should return an error when the array is empty") {
    assertEquals(
      Parser.parser.parseAll("reduce([], ${__value__} + ${__acc__})").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(
        EvaluationError.OperatorRequiresNonEmptyList(
          "reduce",
          List(
            Expr.RList(Nil),
            Expr.RFunction("+", List(Expr.RFunction("var", Expr.RString("__value__")), Expr.RFunction("var", Expr.RString("__acc__")))),
          ),
        ),
      ),
    )
  }

}
