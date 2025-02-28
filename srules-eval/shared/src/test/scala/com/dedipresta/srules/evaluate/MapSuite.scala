package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class MapSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate a map expression on a list of ints") {
    assertEquals(
      Parser.parser.parseAll("map([1,2,3], ${__value__}*2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4), Expr.RInt(6)))),
    )
  }

  test("parse and evaluate a map expression on a list of ints (as expr)") {
    assertEquals(
      Parser.parser.parseAll("map([1,1+1,3], ${__value__}*2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4), Expr.RInt(6)))),
    )
  }

  test("parse and evaluate a map expression on a list of ints that accesses the index variable") {
    assertEquals(
      Parser.parser.parseAll("map([1,1+1,3], ${__index__})").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(0), Expr.RInt(1), Expr.RInt(2)))),
    )
  }

  test("parse and evaluate a map expression on a list of doubles") {
    assertEquals(
      Parser.parser.parseAll("map([1.0,2.0,3.0], ${__value__}*toDouble(5))").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RDouble(5.0), Expr.RDouble(10.0), Expr.RDouble(15.0)))),
    )
  }

}
