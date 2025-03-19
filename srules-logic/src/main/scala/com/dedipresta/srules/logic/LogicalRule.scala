package com.dedipresta.srules.logic

import cats.data.NonEmptyList
import com.dedipresta.srules.Expr

sealed trait LogicalRule
object LogicalRule:
  case class SimpleRule(name: String, rule: Expr) extends LogicalRule
  case class CombinedRules(name: String, combinator: LogicalCombinator, rules: NonEmptyList[LogicalRule]) extends LogicalRule
