package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Floor:

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
          .flatMap(_.withExactly1[F](op))
          .flatMap(floor(op, _).leftMap(_.opError(op, args)).liftTo[F])

      private def floor(op: String, expr: Expr): Either[FailureReason, Expr] =
        expr match {
          case e: Expr.RInt    => Right(e)
          case l: Expr.RLong   => Right(l)
          case Expr.RDouble(d) => Right(Expr.RDouble(d.floor))
          case Expr.RFloat(f)  => Right(Expr.RFloat(f.floor))
          case _               => Left(FailureReason.InvalidArgumentType("Numeric", expr))
        }
