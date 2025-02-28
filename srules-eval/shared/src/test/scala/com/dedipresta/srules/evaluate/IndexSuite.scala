package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import munit.*

final class IndexSuite extends FunSuite {

  given UserContextReader[Map[String, Any]] = UserContextReader.forMapAny(notFoundToNull = true)
  val evaluator                             = new ExprEvaluatorImpl[Map[String, Any]](DefaultOperators.all)
  
  test("parse and evaluate named expression") {
    assertEquals(
      Parser.parser.parseAll("""index()""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.VariableNotFound("__index__")),
    )
  }

}
