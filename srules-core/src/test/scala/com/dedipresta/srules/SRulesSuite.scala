package com.dedipresta.srules

import munit.*

class SRulesSuite extends FunSuite {

  test("SRules.parse parses an expression and return it in an either") {
    assertEquals(
      SRules.parse("1+1"),
      Right(Expr.RFunction("+", List(Expr.RInt(1), Expr.RInt(1)))),
    )
  }

  test("SRules.parseOrThrow parses an expression and return it directly") {
    assertEquals(
      SRules.parseOrThrow("1+1"),
      Expr.RFunction("+", List(Expr.RInt(1), Expr.RInt(1))),
    )
  }

  test("SRules.parseOrThrow parses an expression and return it directly (throw)") {
    intercept[Exception] {
      SRules.parseOrThrow("1+")
    }
  }

}
