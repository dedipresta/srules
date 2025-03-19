package com.dedipresta.srules.logic

sealed trait LogicalCombinator
object LogicalCombinator:
  case object OneOf  extends LogicalCombinator
  case object AllOf  extends LogicalCombinator
  case object NoneOf extends LogicalCombinator
