package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class NonEmptySuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate nonEmpty function on string (true)") {
    assertEquals(
      SRules.parse("""nonEmpty("hello")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate nonEmpty function on string (false)") {
    assertEquals(
      SRules.parse("""nonEmpty("")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate nonEmpty function on list (true)") {
    assertEquals(
      SRules.parse("""nonEmpty([1,2,3])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate nonEmpty function on list (false)") {
    assertEquals(
      SRules.parse("""nonEmpty([])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails to parse nonEmpty function on type that is not string or list") {
    assertEquals(
      SRules.parse("""nonEmpty(1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when no arguments are provided to nonEmpty function") {
    assertEquals(
      SRules.parse("""nonEmpty()""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when more than one argument is provided to nonEmpty function") {
    assertEquals(
      SRules.parse("""nonEmpty("hello", "world")""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
