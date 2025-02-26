package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

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
          .flatTap(_.atLeast(1, op))
          .flatMap { evaluated =>
            if (evaluated.size == 1)
              evaluated match
                // Unary minus operator (value obtained after evaluating the expression)
                case Expr.RInt(a) :: Nil    => Right(Expr.RInt(-a))
                case Expr.RLong(a) :: Nil   => Right(Expr.RLong(-a))
                case Expr.RFloat(a) :: Nil  => Right(Expr.RFloat(-a))
                case Expr.RDouble(a) :: Nil => Right(Expr.RDouble(-a))
                case _                      => Left(EvaluationError.InvalidArgumentType(op, args))
            else
              evaluated match
                case Expr.RInt(a) :: tail    => tail.foldExtract(op, a)(_ - _).map(_.toExpr)
                case Expr.RLong(a) :: tail   => tail.foldExtract(op, a)(_ - _).map(_.toExpr)
                case Expr.RFloat(a) :: tail  => tail.foldExtract(op, a)(_ - _).map(_.toExpr)
                case Expr.RDouble(a) :: tail => tail.foldExtract(op, a)(_ - _).map(_.toExpr)
                case _                       => Left(EvaluationError.InvalidArgumentType(op, args))
          }
