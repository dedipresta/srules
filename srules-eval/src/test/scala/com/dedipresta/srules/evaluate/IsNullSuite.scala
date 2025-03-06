package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class IsNullSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate isNull on null") {
    assertEquals(
      SRules.parse("""isNull(null)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate isNull on string") {
    assertEquals(
      SRules.parse("""isNull("abc")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on list") {
    assertEquals(
      SRules.parse("""isNull([1, 2, 3])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on int") {
    assertEquals(
      SRules.parse("""isNull(1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on long") {
    assertEquals(
      SRules.parse("""isNull(1L)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on float") {
    assertEquals(
      SRules.parse("""isNull(1.0f)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on double") {
    assertEquals(
      SRules.parse("""isNull(1.0d)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on boolean") {
    assertEquals(
      SRules.parse("""isNull(true)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isNull on variable") {
    assertEquals(
      SRules.parse("""isNull($var1)""").flatMap(evaluator.evaluate(_, Map("var1" -> 1.toExpr))),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails when isNull is called with 0 arguments") {
    assertEquals(
      SRules.parse("""isNull()""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when isNull is called with 2 arguments") {
    assertEquals(
      SRules.parse("""isNull(1, 2)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
