package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object NotNull:

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
          .flatMap {
            case Expr.RNull => Right(false.toExpr)
            case other      => Right(true.toExpr)
          }
