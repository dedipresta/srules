package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class VarSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

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

  test("variant notation allows to bypass variable name constraints") {
    assertEquals(
      Parser.parser.parseAll("var(\"some name\")").flatMap(evaluator.evaluate(_, Map("some name" -> 42L))),
      Right(Expr.RLong(42L)),
    )
  }

  test("variant notation allows to give a default value") {
    assertEquals(
      Parser.parser.parseAll("var(\"var1\", 42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(42)),
    )
  }

}
