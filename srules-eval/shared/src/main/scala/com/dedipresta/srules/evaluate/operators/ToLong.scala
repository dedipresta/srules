package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object ToLong:
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
          .flatMap(_.withExactly1(op))
          .flatMap(v => toLong(op, v).map(_.toExpr))

      private def toLong(op: String, expr: Expr): Either[EvaluationError, Long] =
        expr match {
          case Expr.RString(s)  => s.toLongOption.toRight(EvaluationError.InvalidArgument(op, List(expr)))
          case Expr.RInt(i)     => Right(i.toLong)
          case Expr.RBoolean(b) => Right(if (b) 1L else 0L)
          case Expr.RLong(l)    => Right(l)
          case Expr.RDouble(d)  => Right(d.toLong)
          case Expr.RFloat(f)   => Right(f.toLong)
          case _                => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
        }
