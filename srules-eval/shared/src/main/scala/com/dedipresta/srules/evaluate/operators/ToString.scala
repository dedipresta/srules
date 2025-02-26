package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*

import com.dedipresta.srules.*
import com.dedipresta.srules.given
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object ToString:
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
          .flatMap(_.withExactly1(op))
          .map(_.show)
          .map(_.toExpr)


