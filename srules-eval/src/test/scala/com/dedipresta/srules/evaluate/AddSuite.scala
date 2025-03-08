package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class AddSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      SRules.parse("(8-6)+1").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate addition") {
    assertEquals(
      SRules.parse("1+2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate addition with variables") {
    assertEquals(
      SRules.parse("$var1+2").flatMap(evaluator.evaluate(_, Map("var1" -> 1.toExpr))),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate addition between ints") {
    assertEquals(
      SRules.parse("1+2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(3)),
    )
  }

  test("parse and evaluate addition between longs") {
    assertEquals(
      SRules.parse("1L+2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(3L)),
    )
  }

  test("parse and evaluate addition between floats") {
    assertEquals(
      SRules.parse("1.0f+2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(3.0f)),
    )
  }

  test("parse and evaluate addition between doubles") {
    assertEquals(
      SRules.parse("1.0d+2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between int and long") {
    assertEquals(
      SRules.parse("1+2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(3L)),
    )
  }

  test("parse and evaluate addition between int and float") {
    assertEquals(
      SRules.parse("1+2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(3.0f)),
    )
  }

  test("parse and evaluate addition between int and double") {
    assertEquals(
      SRules.parse("1+2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between long and float") {
    assertEquals(
      SRules.parse("1L+2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(3.0f)),
    )
  }

  test("parse and evaluate addition between long and double") {
    assertEquals(
      SRules.parse("1L+2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between long and int") {
    assertEquals(
      SRules.parse("1L+2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(3L)),
    )
  }

  test("parse and evaluate addition between float and double") {
    assertEquals(
      SRules.parse("1.0f+2.0d").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between float and long") {
    assertEquals(
      SRules.parse("1.0f+2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(3.0f)),
    )
  }

  test("parse and evaluate addition between double and float") {
    assertEquals(
      SRules.parse("1.0d+2.0f").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between double and long") {
    assertEquals(
      SRules.parse("1.0d+2L").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between double and int") {
    assertEquals(
      SRules.parse("1.0d+2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(3.0d)),
    )
  }

  test("parse and evaluate addition between float and int") {
    assertEquals(
      SRules.parse("1.0f+2").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(3.0f)),
    )
  }

  test("parse and evaluate addition (concat) between string and string") {
    assertEquals(
      SRules.parse("\"a\"+\"b\"").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("ab")),
    )
  }

  test("parse and evaluate addition with 0 arguments (return RInt(0))") {
    assertEquals(
      SRules.parse("+()").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(0)),
    )
  }

}
