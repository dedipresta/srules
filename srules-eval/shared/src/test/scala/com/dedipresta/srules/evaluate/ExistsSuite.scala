package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ExistsSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate exists function on a list (true)") {
    assertEquals(
      Parser.parser.parseAll("exists([1, 2, 3], value() > 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a list (true computed)") {
    assertEquals(
      Parser.parser.parseAll("exists([1, 2, 1+1+1], value() > 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a list (false)") {
    assertEquals(
      Parser.parser.parseAll("exists([1, 2, 3], value() > 3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
