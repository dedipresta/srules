package com.dedipresta.srules.evaluate

import com.dedipresta.srules.{*, given}
import com.dedipresta.srules.evaluate.operators.*

import cats.syntax.all.*

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import munit.*

final class ExprEvaluatorFutureSuite extends FunSuite {

  import ExprEvaluatorImpl.given
  given UserContextReader[Future, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Future, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all[Future, Map[String, Expr]])

  extension (s: SRules.type) {
    def parseFuture(str: String): Future[Expr] =
      SRules.parse(str).fold(e => Future.failed(new Exception(e.toString)), Future.successful)
  }
  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  val userCtx: Map[String, Expr] = Map[String, Expr](
    "simpleFunction"  -> "1+1".parsed,
    "unevaluatedList" -> "[1, $simpleFunction]".parsed,
  )

  test("evaluator.evaluate with Future (success)") {
    SRules
      .parseFuture("$simpleFunction")
      .flatMap(evaluator.evaluate(_, userCtx))
      .map(_.show)
      .map(assertEquals(_, """(1+1)"""))
  }

  test("evaluator.evaluateAsList[Int] with Future (failure)") {
    SRules
      .parseFuture("1")
      .flatMap(evaluator.evaluateAllAsList[Int](_, userCtx))
      .failed
      .map {
        case EvaluationError.EvaluationException(err) =>
          assertEquals(
            err,
            EvaluationError.OperationFailure("evaluateAsList", List(Expr.RInt(1)), FailureReason.InvalidArgumentType("List", Expr.RInt(1))),
          )
        case _                                        => fail("Expected an EvaluationException")
      }
  }

}
