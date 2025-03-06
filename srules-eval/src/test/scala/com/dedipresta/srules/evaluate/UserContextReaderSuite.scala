package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class UserContextReaderSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]

  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  val value1: Expr = 42L.toExpr
  val value2: Expr = 43L.toExpr

  val data: Map[String, Expr] = Map(
    "var1" -> value1,
  )

  test("UserContextReader.forMapExpr return value when found") {
    val reader: UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)
    assertEquals(reader.read(data, "var1", None), Right(value1))
  }

  test("UserContextReader.forMapExpr return default value when not found") {
    val reader: UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)
    assertEquals(reader.read(data, "var2", Some(value2)), Right(value2))
  }

  test("UserContextReader.forMapExpr return null when not found and not foundToNull is false") {
    val reader: UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)
    assertEquals(reader.read(data, "var2", None), Right(Expr.RNull))
  }

  test("UserContextReader.forMapExpr return error when not found and not foundToNull is false") {
    val reader: UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = false)
    assertEquals(reader.read(data, "var2", None), Left(FailureReason.VariableNotFound("var2").opError("readUserContext", Nil)))
  }

  test("UserContextReader.noContext return default value") {
    val reader: UserContextReader[ErrorOr, Unit] = UserContextReader.noContext(notFoundToNull = true)
    assertEquals(reader.read((), "var1", Some(value1)), Right(value1))
  }

}
