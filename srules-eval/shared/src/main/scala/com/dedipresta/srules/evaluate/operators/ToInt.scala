package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ToInt:

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
          .flatMap(v => toInt(op, v).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private def toInt(op: String, expr: Expr): Either[FailureReason, Int] =
        expr match {
          case Expr.RString(s)  => s.toIntOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Int", expr))
          case Expr.RInt(i)     => Right(i)
          case Expr.RBoolean(b) => Right(if (b) 1 else 0)
          case Expr.RLong(l)    => Right(l.toInt)
          case Expr.RDouble(d)  => Right(d.toInt)
          case Expr.RFloat(f)   => Right(f.toInt)
          case _                => Left(FailureReason.InvalidArgumentType("Cannot be cast to Int", expr))
        }
