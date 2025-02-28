package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Min:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(evaluator: ExprEvaluator[Ctx, EvaluationError], op: String, args: List[Expr], ctx: RuleCtx[Ctx]): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.evaluate(_, ctx))
          .flatMap(_.withAtLeast1(op))
          .flatMap {
            case (Expr.RInt(a), tail)    => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr)
            case (Expr.RLong(a), tail)   => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr)
            case (Expr.RFloat(a), tail)  => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr)
            case (Expr.RDouble(a), tail) => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr)
            case (Expr.RString(a), tail) => tail.foldExtract(a)(_ min _).bimap(_.opError(op, args), _.toExpr)
            case (other, _)              => Left(FailureReason.InvalidArgumentType("Numeric", other)).opError(op, args)
          }
