package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Subtract:

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
          .flatMap(_.withAtLeast1(op))
          .flatMap { (evaluated, tail) =>
            if (tail.isEmpty)
              evaluated match
                // Unary minus operator (value obtained after evaluating the expression)
                case Expr.RInt(a)    => Right(Expr.RInt(-a))
                case Expr.RLong(a)   => Right(Expr.RLong(-a))
                case Expr.RFloat(a)  => Right(Expr.RFloat(-a))
                case Expr.RDouble(a) => Right(Expr.RDouble(-a))
                case other           => Left(FailureReason.InvalidArgumentType("Numeric", other)).opError(op, args)
            else
              evaluated match
                case Expr.RInt(a)    => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr)
                case Expr.RLong(a)   => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr)
                case Expr.RFloat(a)  => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr)
                case Expr.RDouble(a) => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr)
                case other           => Left(FailureReason.InvalidArgumentType("Numeric", other)).opError(op, args)
          }
