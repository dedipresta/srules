package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Round:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.withExactly1(op))
          .flatMap(ceil(op, _).leftMap(_.opError(op, args)))

      private def ceil(op: String, expr: Expr): Either[FailureReason, Expr] =
        expr match {
          case e: Expr.RInt    => Right(e)
          case l: Expr.RLong   => Right(l)
          case Expr.RDouble(d) => Right(Expr.RDouble(d.round.toDouble))
          case Expr.RFloat(f)  => Right(Expr.RFloat(f.round.toFloat))
          case _               => Left(FailureReason.InvalidArgumentType("Numeric", expr))
        }
