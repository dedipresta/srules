package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class VarFromMapAnySuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    Map(
      "+" -> Add(),
      "var" -> VarFromMapAny(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  // floating point var is a test test is specific to runtime (jvm/ js)

  test("parse and evaluate variable expression (long)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42L))),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate variable expression (boolean)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> true))),
      Right(Expr.RBoolean(true)),
    )
  }
  
}
