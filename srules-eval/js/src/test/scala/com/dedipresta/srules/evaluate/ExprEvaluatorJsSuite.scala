package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class ExprEvaluatorJsSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator                              = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  // cast from Any have pitfalls on scala.js runtime

  test("parse and evaluate variable expression (double 0.5d)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 0.5d.toExpr))),
      Right(Expr.RDouble(0.5)),
    )
  }

  test("parse and evaluate variable expression (float 42.0f)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.0f.toExpr))),
      Right(Expr.RFloat(42.0f)),
    )
  }

  test("parse and evaluate variable expression (int 42)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.toExpr))),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate toString function (double)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.0)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.0d")),
    )
  }

  test("parse and evaluate toString function (double with decimal)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.5)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.5d")),
    )
  }

  test("parse and evaluate toString function (float)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.0f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.0f")),
    )
  }

  test("parse and evaluate toString function (float with decimal)") {
    assertEquals(
      Parser.parser.parseAll("toString(42.5f)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RString("42.5f")),
    )
  }

}
