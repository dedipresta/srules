package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

// convert an expression to a float value
object ToFloat:

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
          .flatMap(v => toFloat(op, v).bimap(_.opError(op, args), _.toExpr))

      private def toFloat(op: String, expr: Expr): Either[FailureReason, Float] =
        expr match {
          case Expr.RString(s)  => s.toFloatOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Float", expr))
          case Expr.RInt(i)     => Right(i.toFloat)
          case Expr.RBoolean(b) => Right(if (b) 1f else 0f)
          case Expr.RLong(l)    => Right(l.toFloat)
          case Expr.RDouble(d)  => Right(d.toFloat)
          case Expr.RFloat(f)   => Right(f)
          case _                => Left(FailureReason.InvalidArgumentType("Cannot be cast to Float", expr))
        }
