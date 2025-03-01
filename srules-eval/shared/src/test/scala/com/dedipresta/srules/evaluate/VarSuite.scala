package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class VarSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  // floating point var is a test test is specific to runtime (jvm/ js)

  test("parse and evaluate variable expression (long)") {
    assertEquals(
      SRules.parse("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42L.toExpr))),
      Right(Expr.RLong(42L)),
    )
  }

  test("parse and evaluate variable expression (boolean)") {
    assertEquals(
      SRules.parse("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> true.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("variant notation allows to bypass variable name constraints") {
    assertEquals(
      SRules.parse("var(\"some name\")").flatMap(evaluator.evaluate(_, Map("some name" -> 42L.toExpr))),
      Right(Expr.RLong(42L)),
    )
  }

  test("variant notation allows to give a default value") {
    assertEquals(
      SRules.parse("var(\"var1\", 42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

}
