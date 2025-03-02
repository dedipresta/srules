package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class AtIndexSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("ensure arguments are pre-evaluated") {
    assertEquals(
      Parser.parser
        .parseAll("atIndex($list, $index)")
        .flatMap(evaluator.evaluate(_, Map("list" -> List(1, 2, 3).map(_.toExpr).toExpr, "index" -> 1.toExpr))),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate atIndex on list (value exists)") {
    assertEquals(
      SRules.parse("""atIndex([1, 2, 3], 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate atIndex on list (value does not exist)") {
    assertEquals(
      SRules.parse("""atIndex([1, 2, 3], 3)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("parse and evaluate atIndex on list (value does not exist but default is provided)") {
    assertEquals(
      SRules.parse("""atIndex([1, 2, 3], 3, -1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("parse and evaluate atIndex on string (value exists)") {
    assertEquals(
      SRules.parse("""atIndex("abc", 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("b")),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist)") {
    assertEquals(
      SRules.parse("""atIndex("abc", 3)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist but default is provided)") {
    assertEquals(
      SRules.parse("""atIndex("abc", 3, "z")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("z")),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist but default is provided and is not a string)") {
    assertEquals(
      SRules.parse("""atIndex("abc", 3, 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

}
