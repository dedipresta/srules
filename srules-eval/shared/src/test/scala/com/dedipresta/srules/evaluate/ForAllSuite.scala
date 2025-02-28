package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ForAllSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate a forAll expression (true)") {
    assertEquals(
      Parser.parser.parseAll("forAll([1, 2, 3], ${__value__}>0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate a forAll expression (false)") {
    assertEquals(
      Parser.parser.parseAll("forAll([1, 2, 3], ${__value__}<3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("has a short-circuit evaluation") {
    assertEquals(
      Parser.parser.parseAll("forAll([1, 2, 3/0], ${__value__}>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
