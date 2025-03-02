package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

type EvaluateFn[Ctx, E] = (ExprEvaluator[Ctx, E], String, List[Expr], RuleCtx[Ctx]) => Either[E, Expr]

trait Operator[Ctx, E]:
  def evaluate(
      evaluator: ExprEvaluator[Ctx, E], // reference to the evaluator so that we can evaluate sub-expressions
      op: String,                       // current operator name mostly provided for error messages
      args: List[Expr],                 // arguments of the operator
      ctx: RuleCtx[Ctx],                // context of the evaluation (user context, named variables, current index or value)
  ): Either[E, Expr]
