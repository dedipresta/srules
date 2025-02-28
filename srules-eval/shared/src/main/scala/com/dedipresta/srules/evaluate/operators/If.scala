package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object If:

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
          case _                     => Left(EvaluationError.ZZZZZ(op))
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
          condValue <- evaluator.evaluate(cond, ctx).flatMap(_.withBoolean.leftMap(EvaluationError.OperationFailure(op, List(value), _)))
          result    <- if (condValue) value.asRight else ifElse(evaluator, op, tail, ctx)
          evaluated <- evaluator.evaluate(result, ctx)
        } yield evaluated

      private def ifElse(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args match {
          case h :: s :: subTail => handleConditionAndValue(evaluator, op, h, s, subTail, ctx)
          case last :: Nil       => last.asRight
          case _                 => Left(EvaluationError.ZZZZZ(op))
        }
