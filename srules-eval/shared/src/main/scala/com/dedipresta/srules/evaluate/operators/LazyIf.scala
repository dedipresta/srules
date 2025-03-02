package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object LazyIf:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        // lazy evaluation of the arguments
        // shape of the arguments is: [condition, valueIfTrue, (condition, valueIfTrue)*, valueIfFalse]
        args match {
          case cond :: value :: tail => handleConditionAndValue(evaluator, op, cond, value, tail, ctx)
          case _                     => Left(FailureReason.InvalidArgumentsCountPattern("Arguments count should have shape 3+2n", args.length)).opError(op, args)
        }

      private def handleConditionAndValue(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          cond: Expr,
          value: Expr,
          tail: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        for {
          condValue <- evaluator.evaluatedToBoolean(op, cond, ctx)
          result    <- if (condValue) value.asRight else ifElse(evaluator, op, tail, ctx)
        } yield result

      private def ifElse(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args match {
          case h :: s :: subTail => handleConditionAndValue(evaluator, op, h, s, subTail, ctx)
          case last :: Nil       => last.asRight
          case _                 => Left(FailureReason.InvalidArgumentsCountPattern("Arguments count should have shape 3+2n", args.length)).opError(op, args)
        }
