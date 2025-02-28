package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class MaxSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate max function (ints)") {
    assertEquals(
      Parser.parser.parseAll("max(1,2,3,4)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RInt(4)),
    )
  }

  test("parse and evaluate max function (doubles)") {
    assertEquals(
      Parser.parser.parseAll("max(1.0,2.0,3.0,4.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RDouble(4.0)),
    )
  }

  test("parse and evaluate max function (floats)") {
    assertEquals(
      Parser.parser.parseAll("max(1.0f,2.0f,3.0f,4.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RFloat(4.0f)),
    )
  }

  test("parse and evaluate max function (longs)") {
    assertEquals(
      Parser.parser.parseAll("max(1L,2L,3L,4L)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RLong(4L)),
    )
  }

  test("parse and evaluate max function (strings)") {
    assertEquals(
      Parser.parser.parseAll("max(\"a\",\"b\",\"c\",\"d\")").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("d")),
    )
  }

}
