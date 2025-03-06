package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class ContainsSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      Parser.parser
        .parseAll("contains($list, $index)")
        .flatMap(evaluator.evaluate(_, Map("list" -> List(1, 2, 3).map(_.toExpr).toExpr, "index" -> 1.toExpr))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on ints list (true)""") {
    assertEquals(
      SRules.parse("""contains([1, 2, 3], 2)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on ints  list (false)""") {
    assertEquals(
      SRules.parse("""contains([1, 2, 3], 4)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("""parse and evaluate contains function on strings list (true)""") {
    assertEquals(
      SRules.parse("""contains(["a", "b", "c"], "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on strings list (false)""") {
    assertEquals(
      SRules.parse("""contains(["a", "b", "c"], "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("""parse and evaluate contains function on a string (true)""") {
    assertEquals(
      SRules.parse("""contains("abc", "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on a string (false)""") {
    assertEquals(
      SRules.parse("""contains("abc", "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("fails when searching anon string in a string") {
    assertEquals(
      SRules.parse("""contains("abc", 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

  test("fails when searching in an other type than string or list") {
    assertEquals(
      SRules.parse("""contains(1, 1)""").flatMap(evaluator.evaluate(_, Map.empty)).isLeft,
      true,
    )
  }

}
