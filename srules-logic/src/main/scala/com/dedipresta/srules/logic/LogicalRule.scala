package com.dedipresta.srules.logic

import com.dedipresta.srules.Expr

import cats.data.NonEmptyList

sealed trait LogicalRule
object LogicalRule:
  case class SimpleRule(name: String, rule: Expr)                                                         extends LogicalRule
  case class CombinedRules(name: String, combinator: LogicalCombinator, rules: NonEmptyList[LogicalRule]) extends LogicalRule
