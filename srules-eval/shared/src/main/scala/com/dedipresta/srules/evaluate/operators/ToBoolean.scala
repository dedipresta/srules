package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object ToBoolean:
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
          .flatMap(v => toBoolean(op, v).map(Expr.RBoolean.apply))

      private def toBoolean(op: String, expr: Expr): Either[EvaluationError, Boolean] =
        expr match {
          case Expr.RString(s)  => s.toBooleanOption.toRight(EvaluationError.InvalidArgument(op, List(expr)))
          case Expr.RInt(i)     => Right(i != 0)
          case Expr.RBoolean(b) => Right(b)
          case Expr.RLong(l)    => Right(l != 0L)
          case Expr.RDouble(d)  => Right(d != 0.0)
          case Expr.RFloat(f)   => Right(f != 0.0f)
          case _                => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
        }
