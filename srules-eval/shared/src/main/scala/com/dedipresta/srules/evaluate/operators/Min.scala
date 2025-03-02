package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Min:

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
          .flatMap(_.withAtLeast1[F](op))
          .flatMap {
            case (Expr.RInt(a), tail)    => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (Expr.RLong(a), tail)   => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (Expr.RFloat(a), tail)  => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (Expr.RDouble(a), tail) => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (Expr.RString(a), tail) => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
            case (other, _)              => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, args))
          }
