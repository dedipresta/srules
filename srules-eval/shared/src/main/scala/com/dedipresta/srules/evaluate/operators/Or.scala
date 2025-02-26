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
        // no traverse since we do not want to evaluate all arguments since we want to short-circuit evaluation
        args
          .foldLeft(false.asRight[EvaluationError])((acc, v) =>
            for {
              b     <- acc
              value <- if (b) acc else evaluator.evaluate(v, ctx).flatMap(_.mapBoolean(op, c => b || c))
            } yield value,
          )
          .map(Expr.RBoolean.apply)
