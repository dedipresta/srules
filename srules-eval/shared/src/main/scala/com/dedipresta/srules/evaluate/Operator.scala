package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

trait Operator[F[_], Ctx, E]:
  def evaluate(
      evaluator: ExprEvaluator[F, Ctx, E], // reference to the evaluator so that we can evaluate sub-expressions
      op: String,                          // current operator name mostly provided for error messages
      args: List[Expr],                    // arguments of the operator
      ctx: RuleCtx[Ctx],                   // context of the evaluation (user context, named variables, current index or value)
  ): F[Expr]
