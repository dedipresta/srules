package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ToDouble:

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
          .flatMap(v => toDouble(op, v).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private def toDouble(op: String, expr: Expr): Either[FailureReason, Double] =
        expr match {
          case Expr.RString(s)  => s.toDoubleOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Double", expr))
          case Expr.RInt(i)     => Right(i.toDouble)
          case Expr.RBoolean(b) => Right(if (b) 1.0 else 0.0)
          case Expr.RLong(l)    => Right(l.toDouble)
          case Expr.RDouble(d)  => Right(d)
          case Expr.RFloat(f)   => Right(f.toDouble)
          case _                => Left(FailureReason.InvalidArgumentType("Cannot be cast to Double", expr))
        }
