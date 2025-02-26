package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
  
import munit.*

final class NotSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val not  = Not[Ctx]()
    Map(
      "&&"  -> And(),
      "!"   -> not,
      "not" -> not, // alias
      "+"   -> Add(),
      "var" -> VarFromMapAny(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate not expression") {
    assertEquals(
      Parser.parser.parseAll("!true").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate not expression (comuted)") {
    assertEquals(
      Parser.parser.parseAll("!(true&&false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

}
