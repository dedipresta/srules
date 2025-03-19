package com.dedipresta.srules.logic

trait LogicalEvaluator[F[_], Ctx]:
  def evaluate(rule: LogicalRule, ctx: Ctx): F[Boolean]
  def evaluateWithReport(rule: LogicalRule, ctx: Ctx): F[Report]
