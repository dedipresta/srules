package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object And:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] = new Operator[F, Ctx, EvaluationError]:
    def evaluate(
        evaluator: ExprEvaluator[F, Ctx, EvaluationError],
        op: String,
        args: List[Expr],
        ctx: RuleCtx[Ctx],
    ): F[Expr] =
      // no traverse since we do not want to evaluate all arguments since we want to short-circuit evaluation
      args
        .foldDeepEvaluateWhile(evaluator, op, ctx)(true) { (acc, current) =>
          val and = acc && current
          (and, and)
        }
        .map(_._2.toExpr)
