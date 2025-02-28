package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

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
          .traverse(evaluator.evaluate(_, ctx))
          .flatMap(_.withExactly2(op))
          .flatMap {
            case (Expr.RList(list), value)  => list.indexOf(value).toExpr.asRight
            case (Expr.RString(str), value) => value.mapString(str.indexOf).bimap(_.opError(op, args), _.toExpr)
            case (other, _)                 => Left(FailureReason.InvalidArgumentType("List or String", other)).opError(op, args)
          }
