package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class FindSuite extends FunSuite {
  
  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate a find expression (found)") {
    assertEquals(
      Parser.parser.parseAll("find([1, 2, 3, 4], ${__value__}>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate a find expression (not found)") {
    assertEquals(
      Parser.parser.parseAll("find([1, 2, 3, 4], ${__value__}>4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("has a short-circuit evaluation") {
    assertEquals(
      Parser.parser.parseAll("find([1, 2, 3, 4/0], ${__value__}>2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

}
