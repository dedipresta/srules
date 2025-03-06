package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class NamedSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  test("parse and evaluate named expression") {
    assertEquals(
      SRules.parse("""named("i", $var1, if(isNull(named("i")), -1, named("i")+1))""").flatMap(evaluator.evaluate(_, Map("var1" -> 4.toExpr))),
      Right(Expr.RInt(5)),
    )
  }

  test("parse and evaluate named expression") {
    assertEquals(
      SRules.parse("""named("hello")""").flatMap(evaluator.evaluate(_, Map.empty)),
      Left(EvaluationError.OperationFailure("var", List(Expr.RString("__NAMED__hello")), FailureReason.VariableNotFound("__NAMED__hello"))),
    )
  }

}
