package com.dedipresta.srules.logic

final case class Report(
    name: String,
    satisfied: Boolean,
    combinator: Option[LogicalCombinator],
    details: List[Report],
)
