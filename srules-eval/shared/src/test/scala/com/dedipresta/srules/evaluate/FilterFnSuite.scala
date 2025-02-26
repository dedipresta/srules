package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class FilterFnSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val filterFn        = FilterFn[Ctx]()
    Map(
      "+"        -> Add(),
      "-"        -> Subtract(),
      "*"        -> Multiply(),
      "=="       -> Equal(),
      "%"        -> Mod(),
      "filter"   -> filterFn,
      "var"      -> VarFromMapAny(),
      "toDouble" -> ToDouble(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate a filter expression on a list of ints") {
    assertEquals(
      Parser.parser.parseAll("filter([1,2,3,4], ${__value__} % 2 == 0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))), // even numbers
    )
  }

  test("parse and evaluate a filter expression on a list of ints (as expr)") {
    assertEquals(
      Parser.parser.parseAll("filter([1,6-$var1,3,2*2], ${__value__} % 2 == 0)").flatMap(evaluator.evaluate(_, Map("var1" -> 4))),
      Right(Expr.RList(List(Expr.RInt(2), Expr.RInt(4)))), // even numbers
    )
  }

}
