package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object IndexOf:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .withExactly2(op)
          .flatMap((expr1, expr2) => evaluator.deepEvaluateFunctions(expr1, ctx).map((_, expr2)))
          .flatMap {
            case (Expr.RList(list), value)  => list.indexOf(value).toExpr.asRight
            case (Expr.RString(str), value) => value.mapString(str.indexOf).bimap(_.opError(op, args), _.toExpr)
            case (other, _)                 => Left(FailureReason.InvalidArgumentType("List or String", other)).opError(op, args)
          }
