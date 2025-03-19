package com.dedipresta.srules.logic

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*
import com.dedipresta.srules.logic.*
import com.dedipresta.srules.logic.LogicalCombinator.*

import cats.*
import cats.syntax.all.*

final class LogicalEvaluatorImpl[F[_], Ctx](
    evaluator: ExprEvaluator[F, Ctx, EvaluationError],
)(using F: MonadError[F, EvaluationError])
    extends LogicalEvaluator[F, Ctx] {

  def evaluate(rule: LogicalRule, ctx: Ctx): F[Boolean] =
    rule match {
      case LogicalRule.SimpleRule(name, rule)                 => evaluateAllAs[Boolean](rule, ctx)
      case LogicalRule.CombinedRules(name, combinator, rules) =>
        combinator match {
          case AllOf  => rules.traverse(rule => evaluate(rule, ctx)).map(_.forall(identity))
          case OneOf  => rules.findM[F](rule => evaluate(rule, ctx)).map(_.nonEmpty)
          case NoneOf => rules.traverse(rule => evaluate(rule, ctx)).map(_.forall(!_))
        }
    }

  def evaluateWithReport(rule: LogicalRule, ctx: Ctx): F[Report] =
    rule match {
      case LogicalRule.SimpleRule(name, rule)                 => evaluateAllAs[Boolean](rule, ctx).map(Report(name, _, None, Nil))
      case LogicalRule.CombinedRules(name, combinator, rules) =>
        combinator match {
          case AllOf  =>
            rules.traverse(evaluateWithReport(_, ctx)).map(details => Report(name, details.forall(_.satisfied), Some(AllOf), details.toList))
          case OneOf  =>
            rules.traverse(evaluateWithReport(_, ctx)).map(details => Report(name, details.exists(_.satisfied), Some(OneOf), details.toList))
          case NoneOf =>
            rules.traverse(evaluateWithReport(_, ctx)).map(details => Report(name, details.forall(!_.satisfied), Some(NoneOf), details.toList))
        }
    }

  private def evaluateAllAs[T: ExprExtractor](expr: Expr, ctx: Ctx): F[T] =
    for {
      ev <- evaluator.evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty))
      t  <- summon[ExprExtractor[T]].extract(ev).leftMap(_.opError("evaluateAs", List(ev))).liftTo[F]
    } yield t

}
