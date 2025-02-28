package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class EvalSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate eval expression") {
    assertEquals(
      Parser.parser.parseAll("eval(1 + 1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("eval allows to force evaluation of returned value from a if-else expression") {
    assertEquals(
      Parser.parser.parseAll("eval(if(true, 1 + 1, 5))").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("eval allows to force evaluation of a list sub-expressions") {
    assertEquals(
      Parser.parser.parseAll("eval([1 + 1, 2 + 2])").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))),
    )
  }

  test("eval allows to force evaluation of a list sub-expressions (deep)") {
    assertEquals(
      Parser.parser.parseAll("eval([1 + 1, [2 + 2, 3 + 3]])").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RList(List(Expr.RInt(4), Expr.RInt(6)))))),
    )
  }
}
