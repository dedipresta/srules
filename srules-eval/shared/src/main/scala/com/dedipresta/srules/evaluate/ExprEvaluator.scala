package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

trait ExprEvaluator[F[_], Ctx, E]:
  // real implementation will own a map of operators (key=name, value=Operator)
  // when evaluating an expression, it will extract the operator name retrieve the operator from the map
  // then call operator.evaluate(this, op, args, ctx)

  // lazy version of evaluation, it evaluates only what is needed by operators and does not evaluate list sub-expressions
  final def evaluate(expr: Expr, ctx: Ctx): F[Expr] =
    evaluate(expr, RuleCtx.DefaultContext(ctx, Map.empty))

  // full evaluation, it evaluates all sub-expressions of the expression
  final def evaluateAll(expr: Expr, ctx: Ctx): F[Expr] =
    evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty))

  def evaluate(expr: Expr, ctx: RuleCtx[Ctx]): F[Expr]

  def evaluateAll(expr: Expr, ctx: RuleCtx[Ctx]): F[Expr]
