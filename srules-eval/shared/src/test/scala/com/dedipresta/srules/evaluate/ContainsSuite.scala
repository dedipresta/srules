package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ContainsSuite extends FunSuite {
  
  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)
  
  test("""parse and evaluate contains function on ints list (true)""") {
    assertEquals(
      Parser.parser.parseAll("""contains([1, 2, 3], 2)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on ints  list (false)""") {
    assertEquals(
      Parser.parser.parseAll("""contains([1, 2, 3], 4)""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("""parse and evaluate contains function on strings list (true)""") {
    assertEquals(
      Parser.parser.parseAll("""contains(["a", "b", "c"], "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on strings list (false)""") {
    assertEquals(
      Parser.parser.parseAll("""contains(["a", "b", "c"], "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("""parse and evaluate contains function on a string (true)""") {
    assertEquals(
      Parser.parser.parseAll("""contains("abc", "b")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(true)),
    )
  }

  test("""parse and evaluate contains function on a string (false)""") {
    assertEquals(
      Parser.parser.parseAll("""contains("abc", "d")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

}
