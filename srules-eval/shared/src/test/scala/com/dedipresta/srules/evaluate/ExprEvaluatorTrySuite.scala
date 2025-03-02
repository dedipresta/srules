package com.dedipresta.srules.evaluate

import com.dedipresta.srules.{*, given}
import com.dedipresta.srules.evaluate.operators.*

import cats.syntax.all.*

import scala.util.*

import munit.*

final class ExprEvaluatorTrySuite extends FunSuite {

  import ExprEvaluatorImpl.given
  given UserContextReader[Try, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Try, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all[Try, Map[String, Expr]])

  extension (s: SRules.type) {
    def parseTry(str: String): Try[Expr] =
      SRules.parse(str).fold(e => Failure(new Exception(e.toString)), Success(_))
  }
  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  val userCtx: Map[String, Expr] = Map[String, Expr](
    "simpleFunction"  -> "1+1".parsed,
    "unevaluatedList" -> "[1, $simpleFunction]".parsed,
  )

  test("evaluator.evaluate with Try (success)") {
    assertEquals(
      SRules
        .parseTry("$simpleFunction")
        .flatMap(evaluator.evaluate(_, userCtx))
        .map(_.show),
      Success("""(1+1)"""),
    )
  }

  test("evaluator.evaluateAsList[Int] with Try (failure)") {
    assertEquals(
      SRules
        .parseTry("1")
        .flatMap(evaluator.evaluateAllAsList[Int](_, userCtx)),
      Failure(
        EvaluationError.EvaluationException(
          EvaluationError.OperationFailure("evaluateAsList", List(Expr.RInt(1)), FailureReason.InvalidArgumentType("List", Expr.RInt(1))),
        ),
      ),
    )
  }

}
