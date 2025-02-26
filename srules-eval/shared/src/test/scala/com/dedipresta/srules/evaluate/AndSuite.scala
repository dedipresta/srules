package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class AndSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val and = And[Ctx]()
    Map(
      "and" -> and,
      "&&"  -> and,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate and expression") {
    assertEquals(
      Parser.parser.parseAll("true&&false").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }


  test("and has a short circuit that does not evaluate the second argument") {
    assertEquals(
      Parser.parser.parseAll("false && (1/0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
