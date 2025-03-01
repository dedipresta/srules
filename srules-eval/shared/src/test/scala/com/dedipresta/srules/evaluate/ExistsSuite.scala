package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class ExistsSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("ensure data arguments are pre-evaluated") {
    assertEquals(
      Parser.parser
        .parseAll("exists($list, value() > $index)")
        .flatMap(evaluator.evaluate(_, Map("list" -> List(1, 2, 3).map(_.toExpr).toExpr, "index" -> 1.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("ensure arguments are pre-evaluated (var with simple function)") {
    assertEquals(
      Parser.parser
        .parseAll("exists([1, 2, 3], $f)")
        .flatMap(evaluator.evaluate(_, Map("f" -> Expr.RFunction(">", List(Expr.RInt(2), Expr.RInt(1)))))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("ensure arguments are pre-evaluated (var with function referencing index)") {

    val indexFn = Expr.RFunction("var", "__index__".toExpr)
    val f       = Expr.RFunction("==", List(Expr.RInt(2), indexFn))

    assertEquals(
      Parser.parser
        .parseAll("exists([1, 2, 3], $f)")
        .flatMap(evaluator.evaluate(_, Map("f" -> f))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a list (true)") {
    assertEquals(
      SRules.parse("exists([1, 2, 3], value() > 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a list (true computed)") {
    assertEquals(
      SRules.parse("exists([1, 2, 1+1+1], value() > 2)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a list (false)") {
    assertEquals(
      SRules.parse("exists([1, 2, 3], value() > 3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
