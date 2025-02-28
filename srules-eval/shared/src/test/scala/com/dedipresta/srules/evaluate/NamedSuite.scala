package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class NamedSuite extends FunSuite {
  
  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)

  test("parse and evaluate named expression") {
    assertEquals(
      Parser.parser.parseAll("""named("i", $var1, if(isNull(named("i")), -1, named("i")+1))""").flatMap(evaluator.evaluate(_, Map("var1" -> 4))),
      Right(Expr.RInt(5)),
    )
  }

}
