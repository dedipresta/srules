package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object LazyIf:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        // lazy evaluation of the arguments
        // shape of the arguments is: [condition, valueIfTrue, (condition, valueIfTrue)*, valueIfFalse]
        args match {
          case cond :: value :: tail => handleConditionAndValue(evaluator, op, cond, value, tail, ctx)
          case _                     => F.raiseError(FailureReason.InvalidArgumentsCountPattern("Arguments count should have shape 3+2n", args.length).opError(op, args))
        }

      private def handleConditionAndValue(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          cond: Expr,
          value: Expr,
          tail: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        for {
          condValue <- evaluator.evaluatedToBoolean(op, cond, ctx)
          result    <- if (condValue) value.pure[F] else ifElse(evaluator, op, tail, ctx)
        } yield result

      private def ifElse(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args match {
          case h :: s :: subTail => handleConditionAndValue(evaluator, op, h, s, subTail, ctx)
          case last :: Nil       => last.pure[F]
          case _                 => F.raiseError(FailureReason.InvalidArgumentsCountPattern("Arguments count should have shape 3+2n", args.length).opError(op, args))
        }
