package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object Value:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
                    evaluator: ExprEvaluator[Ctx, EvaluationError],
                    op: String,
                    args: List[Expr],
                    ctx: RuleCtx[Ctx],
                  ): Either[EvaluationError, Expr] =
        args
          .withExactly0(op) // no argument
          .map(_.toValueVar)
          .flatMap(evaluator.evaluate(_, ctx))
