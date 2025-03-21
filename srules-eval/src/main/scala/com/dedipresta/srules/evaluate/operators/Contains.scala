package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Contains:

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
          .flatMap(_.withExactly2[F](op))
          .flatMap {
            case (Expr.RList(list), value)  => list.contains(value).toExpr.pure[F]
            case (Expr.RString(str), value) => value.mapString(str.contains).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (other, _)                 => F.raiseError(FailureReason.InvalidArgumentType("List or String", other).opError(op, args))
          }
