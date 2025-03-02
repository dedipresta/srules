package com.dedipresta.srules

import com.dedipresta.srules.given

import cats.syntax.all.*

import munit.*

class ExprShowSuite extends FunSuite {

  test("a null expression can be serialized") {
    val expr = """null"""
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (add)") {
    val expr = "(1+1)"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (subtract)") {
    val expr = "(1-1)"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (multiply)") {
    val expr = "(1*1)"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (divide)") {
    val expr = "(1/1)"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (modulus)") {
    val expr = "(1%1)"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a complex expression can be parsed and serialized back") {
    val expr = "((1+2)-(((3*4)/5)%6))"

    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a complex expression can be parsed and serialized back (with array)") {
    val expr = "(((1+2)-(((3*4)/5)%6))+[1,2,(3-1),4,5])" // makes no sense, but it's just for testing
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a floating point can be parsed and serialized back (float, no decimal)") {
    val expr = "42f"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right("""42.0f"""),
    )
  }

  test("a floating point can be parsed and serialized back (float, 0 as decimal)") {
    val expr = "42.0f"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a floating point can be parsed and serialized back (float, with decimal)") {
    val expr = "42.5f"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a floating point can be parsed and serialized back (double, no decimal)") {
    val expr = "42.0d"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

  test("a floating point can be parsed and serialized back (double, with decimal)") {
    val expr = "42.5d"
    assertEquals(
      SRules.parse(expr).map(_.show),
      Right(expr),
    )
  }

}
