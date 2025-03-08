package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class IndexOfSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("""parse and evaluate indexOf function on ints list (found)""") {
    assertEquals(
      SRules.parse("""indexOf([1, 2, 3], 2)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("""parse and evaluate indexOf function on ints  list (not found)""") {
    assertEquals(
      SRules.parse("""indexOf([1, 2, 3], 4)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("""parse and evaluate indexOf function on strings list (found)""") {
    assertEquals(
      SRules.parse("""indexOf(["a", "b", "c"], "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("""parse and evaluate indexOf function on strings list (not found)""") {
    assertEquals(
      SRules.parse("""indexOf(["a", "b", "c"], "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("""parse and evaluate indexOf function on a string (found)""") {
    assertEquals(
      SRules.parse("""indexOf("abc", "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

  test("""parse and evaluate indexOf function on a string (not found)""") {
    assertEquals(
      SRules.parse("""indexOf("abc", "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("fails when searching indexOf on a string with a non-string") {
    assertEquals(
      SRules.parse("""indexOf("abc", 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when searching indexOf on a type that is neither string or list") {
    assertEquals(
      SRules.parse("""indexOf(1, 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
