package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class ConcatSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]

  given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)

  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      Parser.parser
        .parseAll("concat($list, [4, 5, 6])")
        .flatMap(evaluator.evaluate(_, Map("list" -> List(1, 2, 3).map(_.toExpr).toExpr))),
      Right(Expr.RList(List(1, 2, 3, 4, 5, 6).map(_.toExpr))),
    )
  }

  test("""parse and evaluate concat function with 2 lists""") {
    assertEquals(
      Parser.parser
        .parseAll("concat([1, 2, 3], [4, 5, 6])")
        .flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(1, 2, 3, 4, 5, 6).map(_.toExpr))),
    )
  }

  test("""parse and evaluate concat function with 3 lists""") {
    assertEquals(
      Parser.parser
        .parseAll("concat([1, 2, 3], [4, 5, 6], [7, 8, 9])")
        .flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RList(List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(_.toExpr))),
    )
  }

  test("""parse and evaluate concat function with 2 strings""") {
    assertEquals(
      Parser.parser
        .parseAll("""concat("hello", "world")""")
        .flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("helloworld")),
    )
  }

  test("""parse and evaluate concat function with 3 strings""") {
    assertEquals(
      Parser.parser
        .parseAll("""concat("hello", "world", "!")""")
        .flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("helloworld!")),
    )
  }

  test("fails when no arguments are provided to concat function") {
    assertEquals(
      Parser.parser
        .parseAll("concat()")
        .flatMap(evaluator.evaluate(_, Map.empty))
        .isLeft,
      true,
    )
  }

  test("fails when trying to add a string to a list") {
    assertEquals(
      Parser.parser
        .parseAll("""concat([1, 2, 3], "hello")""")
        .flatMap(evaluator.evaluate(_, Map.empty))
        .isLeft,
      true,
    )
  }

  test("fails when trying to add a list to a string") {
    assertEquals(
      Parser.parser
        .parseAll("""concat("hello", [1, 2, 3])""")
        .flatMap(evaluator.evaluate(_, Map.empty))
        .isLeft,
      true,
    )
  }

  test("fails when first argument is neither a string or a list") {
    assertEquals(
      Parser.parser
        .parseAll("""concat(1, [1, 2, 3])""")
        .flatMap(evaluator.evaluate(_, Map.empty))
        .isLeft,
      true,
    )
  }

}
