package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class VarSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate variable expression (int)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.toExpr))),
      Right(Expr.RInt(42)),
    )
  }

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

  test("parse and evaluate variable expression (double)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 0.5d.toExpr))),
      Right(Expr.RDouble(0.5)),
    )
  }

  test("parse and evaluate variable expression (float)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.0f.toExpr))),
      Right(Expr.RFloat(42.0f)),
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

  test("fails when argument is not a string") {
    assertEquals(
      SRules.parse("""var(1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
