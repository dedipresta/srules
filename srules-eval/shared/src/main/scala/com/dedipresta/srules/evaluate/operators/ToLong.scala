package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ToLong:
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
          .flatMap(v => toLong(op, v).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private def toLong(op: String, expr: Expr): Either[FailureReason, Long] =
        expr match {
          case Expr.RString(s)  => s.toLongOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Long", expr))
          case Expr.RInt(i)     => Right(i.toLong)
          case Expr.RBoolean(b) => Right(if (b) 1L else 0L)
          case Expr.RLong(l)    => Right(l)
          case Expr.RDouble(d)  => Right(d.toLong)
          case Expr.RFloat(f)   => Right(f.toLong)
          case _                => Left(FailureReason.InvalidArgumentType("Cannot be cast to Long", expr))
        }
