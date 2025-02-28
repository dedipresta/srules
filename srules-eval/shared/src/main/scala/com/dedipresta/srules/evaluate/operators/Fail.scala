package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Fail:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(evaluator: ExprEvaluator[Ctx, EvaluationError], op: String, args: List[Expr], ctx: RuleCtx[Ctx]): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.evaluate(_, ctx))
          .flatMap(_.withOptional(op))
          .flatMap {
            case None                  => Left(EvaluationError.OperationFailure(op, args, FailureReason.Message("Fail operator called")))
            case Some(Expr.RString(s)) => Left(EvaluationError.OperationFailure(op, args, FailureReason.Message(s)))
            case Some(other)           => Left(EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("String", other)))
          }
