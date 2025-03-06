package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object IndexOf:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .withExactly2[F](op)
          .flatMap((expr1, expr2) => evaluator.deepEvaluateFunctions(expr1, ctx).map((_, expr2)))
          .flatMap {
            case (Expr.RList(list), value)  => list.indexOf(value).toExpr.pure[F]
            case (Expr.RString(str), value) => value.mapString(str.indexOf).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (other, _)                 => F.raiseError(FailureReason.InvalidArgumentType("List or String", other).opError(op, args))
          }
