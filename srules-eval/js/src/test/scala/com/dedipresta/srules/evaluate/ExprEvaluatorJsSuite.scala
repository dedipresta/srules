package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class ExprEvaluatorJsSuite extends FunSuite {

  type Ctx = Map[String, Any]
  
  private val operators = {
    val not = Not[Ctx]()
    val and = And[Ctx]()
    // lt, lte, gt, gte could be refactored since they are all similar // TODO check Gt since it has been partially refactored
    val lt  = Lt[Ctx]()
    val lte = Lte[Ctx]()
    val gt  = Gt[Ctx]()
    val gte = Gte[Ctx]()

    Map[String, Operator[Ctx, EvaluationError]](
      "var"       -> VarFromMapAny(),
      "toString"  -> ToString(),
    )
  }

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  // cast from ANy have pitfalls on scala.js runtime

  test("parse and evaluate variable expression (double 0.5d => float)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 0.5d))),
      Right(Expr.RFloat(0.5f)),
    )
  }

  test("parse and evaluate variable expression (double 42.0d => int)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.0d))),
      Right(Expr.RInt(42)),
    )
  }

  test("parse and evaluate variable expression (float 42.0d => int)") {
    assertEquals(
      Parser.parser.parseAll("$var1").flatMap(evaluator.evaluate(_, Map("var1" -> 42.0f))),
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
