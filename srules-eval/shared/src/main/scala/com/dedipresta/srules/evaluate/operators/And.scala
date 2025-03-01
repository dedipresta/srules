package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object And:

  def apply[Ctx](): Operator[Ctx, EvaluationError] = new Operator[Ctx, EvaluationError]:
    def evaluate(
        evaluator: ExprEvaluator[Ctx, EvaluationError],
        op: String,
        args: List[Expr],
        ctx: RuleCtx[Ctx],
    ): Either[EvaluationError, Expr] =
      // no traverse since we do not want to evaluate all arguments since we want to short-circuit evaluation
      args
        .foldDeepEvaluateWhile(evaluator, op, ctx)(true) { (acc, current) =>
          val and = acc && current
          (and, and)
        }
        .map(_._2.toExpr)
