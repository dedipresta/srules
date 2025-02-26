package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

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
          .traverse(evaluator.evaluate(_, ctx))
          .flatTap(_.exactly(1, op))
          .flatMap(v => floor(op, v.head))

      private def floor(op: String, expr: Expr): Either[EvaluationError, Expr] =
        expr match {
          case e: Expr.RInt    => Right(e)
          case l: Expr.RLong   => Right(l)
          case Expr.RDouble(d) => Right(Expr.RDouble(d.floor))
          case Expr.RFloat(f)  => Right(Expr.RFloat(f.floor))
          case _               => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
        }
