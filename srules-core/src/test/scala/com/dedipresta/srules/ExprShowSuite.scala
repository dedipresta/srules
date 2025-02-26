package com.dedipresta.srules

import cats.syntax.all.*
import com.dedipresta.srules.given
import munit.*

class ExprShowSuite extends FunSuite {

  test("an expression can be parsed and serialized back (add)") {
    val expr = "(1+1)"
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (subtract)") {
    val expr = "(1-1)"
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (multiply)") {
    val expr = "(1*1)"
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (divide)") {
    val expr = "(1/1)"
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("an expression can be parsed and serialized back (modulus)") {
    val expr = "(1%1)"
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("a complex expression can be parsed and serialized back") {
    val expr = "((1+2)-(((3*4)/5)%6))"

    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

  test("a complex expression can be parsed and serialized back (with array)") {
    val expr = "(((1+2)-(((3*4)/5)%6))+[1,2,(3-1),4,5])" // makes no sense, but it's just for testing
    assertEquals(
      Parser.parser.parseAll(expr).map(_.show),
      Right(expr),
    )
  }

}
