package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object ToInt:
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
          .flatMap(v => toInt(op, v).map(_.toExpr))

      private def toInt(op: String, expr: Expr): Either[EvaluationError, Int] =
        expr match {
          case Expr.RString(s)  => s.toIntOption.toRight(EvaluationError.InvalidArgument(op, List(expr)))
          case Expr.RInt(i)     => Right(i)
          case Expr.RBoolean(b) => Right(if (b) 1 else 0)
          case Expr.RLong(l)    => Right(l.toInt)
          case Expr.RDouble(d)  => Right(d.toInt)
          case Expr.RFloat(f)   => Right(f.toInt)
          case _                => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
        }
