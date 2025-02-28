package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class PowSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate pow function on ints (4^2)") {
    assertEquals(
      Parser.parser.parseAll("pow(4, 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(16.0)),
    )
  }

  test("parse and evaluate pow function on doubles (4.0^2.0)") {
    assertEquals(
      Parser.parser.parseAll("pow(4.0, 2.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(16.0)),
    )
  }

}
