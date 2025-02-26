package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class GteSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val gte   = Gte[Ctx]()
    Map(
      "+"   -> Add(),
      ">="  -> gte,
      "gte" -> gte, // alias
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate gte function (ints true)") {
    assertEquals(
      Parser.parser.parseAll("gte(4,3,2,1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate gte function (ints false)") {
    assertEquals(
      Parser.parser.parseAll("gte(4,3,2,3)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
