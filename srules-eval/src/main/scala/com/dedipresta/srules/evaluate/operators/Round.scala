package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Round:

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
          .flatMap(roundFn(_).leftMap(_.opError(op, args)).liftTo[F])

      private def roundFn: Expr => Either[FailureReason, Expr] =
        case e: Expr.RInt    => Right(e)
        case l: Expr.RLong   => Right(l)
        case Expr.RDouble(d) => Right(Expr.RDouble(d.round.toDouble))
        case Expr.RFloat(f)  => Right(Expr.RFloat(f.round.toFloat))
        case other           => Left(FailureReason.InvalidArgumentType("Numeric", other))
