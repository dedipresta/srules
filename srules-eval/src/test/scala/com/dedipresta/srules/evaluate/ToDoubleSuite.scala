package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ToDoubleSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate toDouble function (int)") {
    assertEquals(
      SRules.parse("toDouble(42)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (string)") {
    assertEquals(
      SRules.parse("toDouble(\"42\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (double)") {
    assertEquals(
      SRules.parse("toDouble(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (float)") {
    assertEquals(
      SRules.parse("toDouble(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (long)") {
    assertEquals(
      SRules.parse("toDouble(42L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(42.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean)") {
    assertEquals(
      SRules.parse("toDouble(true)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(1.0)),
    )
  }

  test("parse and evaluate toDouble function (boolean false)") {
    assertEquals(
      SRules.parse("toDouble(false)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(0.0)),
    )
  }

  test("fails when argument is a list") {
    assertEquals(
      SRules.parse("toDouble([1,2,3])").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
