package com.dedipresta.srules.evaluate

import com.dedipresta.srules.{*, given}
import com.dedipresta.srules.evaluate.operators.*

import cats.MonadError
import cats.syntax.all.*

import scala.util.*

import munit.*

final class ExprEvaluatorTrySuite extends FunSuite {

  import ExprEvaluatorImpl.given
  given UserContextReader[Try, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = false)
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

  test("evaluator.evaluate with Try allow recovering from failure") {
    assertEquals(
      SRules
        .parseTry("$notFound")
        .flatMap(evaluator.evaluate(_, userCtx))
        .toEither
        .swap
        .toOption
        .flatMap {
          case EvaluationError.EvaluationException(err) => Some(err)
          case _                                        => None
        },
      Some(EvaluationError.OperationFailure("var", List(Expr.RString("notFound")), FailureReason.VariableNotFound("notFound"))),
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

  test("monad error have a tailRecM") {

    val instance = summon[MonadError[Try, EvaluationError]]

    val result: Try[Int] = instance.tailRecM(0) { current =>
      if (current < 5) Try(Left(current + 1))
      else Try(Right(current))
    }

    assertEquals(result, Success(5))
  }

}
