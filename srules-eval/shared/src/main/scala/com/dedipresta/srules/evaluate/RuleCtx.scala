package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

sealed trait RuleCtx[Ctx]:
  def user: Ctx
  def named: Map[String, Expr]
object RuleCtx:
  case class DefaultContext[Ctx](user: Ctx, named: Map[String, Expr])                                        extends RuleCtx[Ctx]
  case class WithIndexedValue[Ctx](user: Ctx, named: Map[String, Expr], index: Int, current: Expr)           extends RuleCtx[Ctx]
  case class WithAccumulator[Ctx](user: Ctx, named: Map[String, Expr], index: Int, current: Expr, acc: Expr) extends RuleCtx[Ctx]

  extension [RCtx](r: RuleCtx[RCtx]) {
    def withIndexedValue(index: Int, expr: Expr): WithIndexedValue[RCtx] =
      r match
        case c: DefaultContext[RCtx]   => WithIndexedValue(c.user, c.named, index, expr)
        case c: WithIndexedValue[RCtx] => WithIndexedValue(c.user, c.named, index, expr)
        case c: WithAccumulator[RCtx]  => WithIndexedValue(c.user, c.named, index, expr)

    def withAccumulator(index: Int, current: Expr, acc: Expr): WithAccumulator[RCtx] =
      r match
        case c: DefaultContext[RCtx]   => WithAccumulator(c.user, c.named, index, current, acc)
        case c: WithIndexedValue[RCtx] => WithAccumulator(c.user, c.named, index, current, acc)
        case c: WithAccumulator[RCtx]  => WithAccumulator(c.user, c.named, index, current, acc)

    def currentValue: Option[Expr] =
      r match
        case c: DefaultContext[RCtx]   => None
        case c: WithIndexedValue[RCtx] => Some(c.current)
        case c: WithAccumulator[RCtx]  => Some(c.current)

    def indexValue: Option[Int] =
      r match
        case c: DefaultContext[RCtx]   => None
        case c: WithIndexedValue[RCtx] => Some(c.index)
        case c: WithAccumulator[RCtx]  => Some(c.index)

    def accumulatorValue: Option[Expr] =
      r match
        case c: WithAccumulator[RCtx] => Some(c.acc)
        case _                        => None

    def namedValue(name: String): Option[Expr] =
      r.named.get(name)

    def addNamedValue(name: String, value: Expr): RuleCtx[RCtx] =
      r match
        case c: DefaultContext[RCtx]   => c.copy(named = c.named + (name -> value))
        case c: WithIndexedValue[RCtx] => c.copy(named = c.named + (name -> value))
        case c: WithAccumulator[RCtx]  => c.copy(named = c.named + (name -> value))

  }
