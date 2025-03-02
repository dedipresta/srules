package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

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
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.withExactly1(op))
          .flatMap(v => toBoolean(op, v).bimap(_.opError(op, args), _.toExpr))

      private def toBoolean(op: String, expr: Expr): Either[FailureReason, Boolean] =
        expr match {
          case Expr.RString(s)  => s.toBooleanOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Boolean", expr))
          case Expr.RInt(i)     => Right(i != 0)
          case Expr.RBoolean(b) => Right(b)
          case Expr.RLong(l)    => Right(l != 0L)
          case Expr.RDouble(d)  => Right(d != 0.0)
          case Expr.RFloat(f)   => Right(f != 0.0f)
          case _                => Left(FailureReason.InvalidArgumentType("Cannot be cast to Boolean", expr))
        }
