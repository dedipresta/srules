package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ExistsSuite extends FunSuite {

  type Ctx = Map[String, Any]

  val operators: Map[String, Operator[Ctx, EvaluationError]] =
    val not      = Not[Ctx]()
    Map(
      "!"      -> not,
      "not"    -> not, // alias
      "+"      -> Add(),
      "var"    -> VarFromMapAny(),
      "exists" -> Exists(),
    )

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)

  test("parse and evaluate exists function on a variable that exists") {
    assertEquals(
      Parser.parser.parseAll("exists($var1)").flatMap(evaluator.evaluate(_, Map("var1" -> 4))),
      Right(Expr.RBoolean(true)),
    )
  }

  test("parse and evaluate exists function on a variable that does not exist") {
    assertEquals(
      Parser.parser.parseAll("exists($var1)").flatMap(evaluator.evaluate(_, Map.empty)),
      Right(Expr.RBoolean(false)),
    )
  }

  test("parse and evaluate !exists function on a variable that exists") {
    assertEquals(
      Parser.parser.parseAll("not(exists($var1))").flatMap(evaluator.evaluate(_, Map("var1" -> 4))),
      Right(Expr.RBoolean(false)),
    )
  }

}
