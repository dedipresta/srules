package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Or:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        // no traverse since we want to short-circuit evaluation as soon as we find a true value
        args
          .foldLeft(false.asRight[EvaluationError])((acc, v) => // neutral element for OR is false
            acc.flatMap {
              case true => acc // bool condition is true, no need to evaluate the rest of the arguments
              case _    =>
                // evaluate before use
                evaluator
                  .evaluate(v, ctx)
                  // read it as boolean (it becomes the next acc value ; false || other = other)
                  .flatMap(_.withBoolean.leftMap(EvaluationError.OperationFailure(op, args, _)))
            },
          )
          .map(_.toExpr)
