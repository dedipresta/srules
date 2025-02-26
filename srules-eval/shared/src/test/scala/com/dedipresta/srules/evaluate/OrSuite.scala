package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class OrSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val or = Or[Ctx]()
    Map(
      "or" -> or,
      "||"  -> or,
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate or expression") {
    assertEquals(
      Parser.parser.parseAll("true||false").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }
  
  test("or has a short circuit that does not evaluate the second argument") {
    assertEquals(
      Parser.parser.parseAll("true || (1/0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

}
