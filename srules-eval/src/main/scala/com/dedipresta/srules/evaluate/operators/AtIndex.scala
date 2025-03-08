package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object AtIndex:

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
          .flatMap(_.with2Or3(op))
          .flatMap {
            case (Expr.RList(list), Expr.RInt(index), default)  =>
              list.lift(index).orElse(default).getOrElse(Expr.RNull).pure[F]
            case (Expr.RString(str), Expr.RInt(index), default) =>
              str.lift(index).map(_.toString.toExpr).orElse(default).getOrElse(Expr.RNull).pure[F]
            case (other, _, _)                                  =>
              F.raiseError(
                EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("List or String, Int, and optional expression", other)),
              )
          }
