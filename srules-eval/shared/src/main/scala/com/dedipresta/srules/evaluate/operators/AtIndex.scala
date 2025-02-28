package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object AtIndex:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.evaluate(_, ctx))
          .flatMap(_.with2Or3(op))
          .flatMap {
            case (Expr.RList(list), Expr.RInt(index), default)  =>
              list.lift(index).orElse(default).getOrElse(Expr.RNull).asRight
            case (Expr.RString(str), Expr.RInt(index), default) =>
              str.lift(index).map(_.toString.toExpr).orElse(default).getOrElse(Expr.RNull).asRight
            case (other, _, _)                                  =>
              Left(EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("List or String, Int, and optional expression", other)))
          }
