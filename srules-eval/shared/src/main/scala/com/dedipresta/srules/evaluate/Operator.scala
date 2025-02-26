package com.dedipresta.srules.evaluate

import cats.syntax.all.*

import com.dedipresta.srules.Expr

// TODO evaluated/ lazy
// Effect
// alias
// provide name or operator itself ?

type EvaluateFn[Ctx, E] = (ExprEvaluator[Ctx, E], String, List[Expr], RuleCtx[Ctx]) => Either[E, Expr]

trait Operator[Ctx, E]:
  def evaluate(
      evaluator: ExprEvaluator[Ctx, E],
      op: String,
      args: List[Expr],
      ctx: RuleCtx[Ctx],
  ): Either[E, Expr]

object Operator:

    def evaluated[Ctx, E](f: EvaluateFn[Ctx, E]): Operator[Ctx, E] =
      new Operator[Ctx, E]:
        def evaluate(
                      evaluator: ExprEvaluator[Ctx, E],
                      op: String,
                      args: List[Expr],
                      ctx: RuleCtx[Ctx],
                    ): Either[E, Expr] =
          args
            .traverse(evaluator.evaluate(_, ctx))
            .flatMap(f(evaluator, op, _, ctx))

final case class NamedOperator[Ctx, E](
    name: String,
    operator: Operator[Ctx, E],
    aliases: Set[String]
)
object NamedOperator:
  extension [Ctx,E](no: NamedOperator[Ctx, E]) {
    def addAlias(s: String): NamedOperator[Ctx, E] = no.copy(aliases = no.aliases + s)
    def addAliases(s: String*): NamedOperator[Ctx,E] = no.copy(aliases = no.aliases ++ s.toSet)
  }