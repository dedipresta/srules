package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class GtSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val gt  = Gt[Ctx]()
    Map(
      "+"  -> Add(),
      "gt" -> gt,
      ">"  -> gt,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate gt function (ints true)") {
    assertEquals(
      Parser.parser.parseAll("gt(4,3,2,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gt function (inner evaluation required)") {
    assertEquals(
      Parser.parser.parseAll("gt(4,3,1+1,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gt function (ints false)") {
    assertEquals(
      Parser.parser.parseAll("gt(4,3,2,3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
