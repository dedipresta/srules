package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class NamedSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  test("parse and evaluate named expression") {
    assertEquals(
      SRules.parse("""named("i", $var1, if(isNull(named("i")), -1, named("i")+1))""").flatMap(evaluator.evaluate(_, Map("var1" -> 4.toExpr))),
      Right(Expr.RInt(5)),
    )
  }

}
