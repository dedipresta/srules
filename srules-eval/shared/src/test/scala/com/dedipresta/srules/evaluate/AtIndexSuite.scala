package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class AtIndexSuite extends FunSuite {
  
  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)
  
  test("parse and evaluate atIndex on list (value exists)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex([1, 2, 3], 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(2)),
    )
  }

  test("parse and evaluate atIndex on list (value does not exist)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex([1, 2, 3], 3)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("parse and evaluate atIndex on list (value does not exist but default is provided)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex([1, 2, 3], 3, -1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(-1)),
    )
  }

  test("parse and evaluate atIndex on string (value exists)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex("abc", 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("b")),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex("abc", 3)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RNull),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist but default is provided)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex("abc", 3, "z")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("z")),
    )
  }

  test("parse and evaluate atIndex on string (value does not exist but default is provided and is not a string)") {
    assertEquals(
      Parser.parser.parseAll("""atIndex("abc", 3, 1)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(1)),
    )
  }

}
