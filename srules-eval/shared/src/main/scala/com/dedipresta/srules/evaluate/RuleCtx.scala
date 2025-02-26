package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

sealed trait RuleCtx[Ctx]:
  def userCtx: Ctx
object RuleCtx:
  case class SimpleContext[Ctx](userCtx: Ctx)                                         extends RuleCtx[Ctx]
  case class WithIndexedValue[Ctx](userCtx: Ctx, index: Int, current: Expr)           extends RuleCtx[Ctx]
  case class WithAccumulator[Ctx](userCtx: Ctx, index: Int, current: Expr, acc: Expr) extends RuleCtx[Ctx]

  extension [RCtx](r: RuleCtx[RCtx]) {
    def withIndexedValue(index: Int, expr: Expr): WithIndexedValue[RCtx] =
      WithIndexedValue(r.userCtx, index, expr)

    def withAccumulator(index: Int, current: Expr, acc: Expr): WithAccumulator[RCtx] =
      WithAccumulator(r.userCtx, index, current, acc)

    def currentValue: Option[Expr] =
      r match
        case WithIndexedValue(_, _, current)   => Some(current)
        case WithAccumulator(_, _, current, _) => Some(current)
        case _                                 => None

    def indexValue: Option[Int] =
      r match
        case WithIndexedValue(_, index, _)   => Some(index)
        case WithAccumulator(_, index, _, _) => Some(index)
        case _                               => None

    def accumulatorValue: Option[Expr] =
      r match
        case WithAccumulator(_, _, _, acc) => Some(acc)
        case _                             => None

  }
