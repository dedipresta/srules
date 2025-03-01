package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Floor:

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
          .flatMap(floor(op, _).leftMap(_.opError(op, args)))

      private def floor(op: String, expr: Expr): Either[FailureReason, Expr] =
        expr match {
          case e: Expr.RInt    => Right(e)
          case l: Expr.RLong   => Right(l)
          case Expr.RDouble(d) => Right(Expr.RDouble(d.floor))
          case Expr.RFloat(f)  => Right(Expr.RFloat(f.floor))
          case _               => Left(FailureReason.InvalidArgumentType("Numeric", expr))
        }
