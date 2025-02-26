package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object ToDouble:
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
          .flatMap(v => toDouble(op, v).map(_.toExpr))

      private def toDouble(op: String, expr: Expr): Either[EvaluationError, Double] =
        expr match {
          case Expr.RString(s)  => s.toDoubleOption.toRight(EvaluationError.InvalidArgument(op, List(expr)))
          case Expr.RInt(i)     => Right(i.toDouble)
          case Expr.RBoolean(b) => Right(if (b) 1.0 else 0.0)
          case Expr.RLong(l)    => Right(l.toDouble)
          case Expr.RDouble(d)  => Right(d)
          case Expr.RFloat(f)   => Right(f.toDouble)
          case _                => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
        }
