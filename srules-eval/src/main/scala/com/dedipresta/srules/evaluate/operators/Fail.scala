package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Fail:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.withOptional(op))
          .flatMap {
            case None                  => F.raiseError(EvaluationError.OperationFailure(op, args, FailureReason.Message("Fail operator called")))
            case Some(Expr.RString(s)) => F.raiseError(EvaluationError.OperationFailure(op, args, FailureReason.Message(s)))
            case Some(other)           => F.raiseError(EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("String", other)))
          }
