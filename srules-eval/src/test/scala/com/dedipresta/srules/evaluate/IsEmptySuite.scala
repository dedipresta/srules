package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class IsEmptySuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate isEmpty on string (true)") {
    assertEquals(
      SRules.parse("""isEmpty("")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate isEmpty on string (false)") {
    assertEquals(
      SRules.parse("""isEmpty("abc")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate isEmpty on list (true)") {
    assertEquals(
      SRules.parse("""isEmpty([])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate isEmpty on list (false)") {
    assertEquals(
      SRules.parse("""isEmpty([1, 2, 3])""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails when isEmpty is called with a non-list or non-string") {
    assertEquals(
      SRules.parse("""isEmpty(1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when providing 0 arguments") {
    assertEquals(
      SRules.parse("""isEmpty()""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when providing 2 arguments") {
    assertEquals(
      SRules.parse("""isEmpty("1", 2)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
