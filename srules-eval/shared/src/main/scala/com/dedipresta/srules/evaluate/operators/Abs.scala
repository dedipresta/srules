package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Abs:

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
          .flatMap(abs(op, _).leftMap(_.opError(op, args)))

      private def abs(op: String, expr: Expr): Either[FailureReason, Expr] =
        expr match {
          case e: Expr.RInt    => Right(Expr.RInt(Math.abs(e.value)))
          case l: Expr.RLong   => Right(Expr.RLong(Math.abs(l.value)))
          case Expr.RDouble(d) => Right(Expr.RDouble(Math.abs(d)))
          case Expr.RFloat(f)  => Right(Expr.RFloat(Math.abs(f)))
          case _               => Left(FailureReason.InvalidArgumentType("Numeric", expr))
        }
