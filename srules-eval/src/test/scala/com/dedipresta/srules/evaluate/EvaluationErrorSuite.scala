package com.dedipresta.srules.evaluate

import com.dedipresta.srules.{*, given}
import com.dedipresta.srules.evaluate.operators.*

import cats.syntax.all.*

import munit.*

final class EvaluationErrorSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  val expr: Expr         = "1+1".parsed
  val exprString: String = expr.show
  val left: Expr         = "1".parsed
  val right: Expr        = "0".parsed

  test("OperatorNotFound includes the name of the operator in its message") {
    assertEquals(
      EvaluationError.OperatorNotFound("foo", expr).message.contains("foo"),
      true,
    )
  }

  test("OperatorNotFound includes the faulty expression in its message") {
    assertEquals(
      EvaluationError.OperatorNotFound("foo", expr).message.contains(exprString),
      true,
    )
  }

  test("OperationFailure includes the name of the operator in its message") {
    assertEquals(
      EvaluationError.OperationFailure("foo", List(expr), FailureReason.Message("bar")).message.contains("foo"),
      true,
    )
  }

  test("OperationFailure includes the faulty expression in its message") {
    assertEquals(
      EvaluationError.OperationFailure("foo", List(expr), FailureReason.Message("bar")).message.contains(exprString),
      true,
    )
  }

  test("EvaluationError can be transformed to an exception") {
    val error: EvaluationError = EvaluationError.OperationFailure("foo", List(expr), FailureReason.Message("bar"))
    val exception: Exception   = error.toException
    assertEquals(exception.isInstanceOf[Exception], true)
    assertEquals(exception.getMessage.contains("foo"), true)
    assertEquals(exception.getMessage.contains(exprString), true)
  }

  test("FailureReason.Message includes the message in its message") {
    assertEquals(
      FailureReason.Message("foo").message.contains("foo"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentType includes the expected type in its message") {
    assertEquals(
      FailureReason.InvalidArgumentType("foo", expr).message.contains("foo"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentType includes the faulty expression in its message") {
    assertEquals(
      FailureReason.InvalidArgumentType("foo", expr).message.contains(exprString),
      true,
    )
  }

  test("FailureReason.InvalidArgumentTypes includes the expected types in its message") {
    assertEquals(
      FailureReason.InvalidArgumentTypes("foo", List(expr)).message.contains("foo"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentTypes includes the faulty expressions in its message") {
    assertEquals(
      FailureReason.InvalidArgumentTypes("foo", List(expr)).message.contains(exprString),
      true,
    )
  }

  test("FailureReason.InvalidArgumentValue includes info about the expected value in its message") {
    assertEquals(
      FailureReason.InvalidArgumentValue("foo", expr).message.contains("foo"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentValue includes the faulty expression in its message") {
    assertEquals(
      FailureReason.InvalidArgumentValue("foo", expr).message.contains(exprString),
      true,
    )
  }

  test("FailureReason.InvalidArgumentsCount includes the expected min count in its message") {
    assertEquals(
      FailureReason.InvalidArgumentsCount(Some(1), None, 0).message.contains("min=1"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentsCount includes the expected max count in its message") {
    assertEquals(
      FailureReason.InvalidArgumentsCount(None, Some(1), 2).message.contains("max=1"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentsCount includes the actual count in its message") {
    assertEquals(
      FailureReason.InvalidArgumentsCount(None, None, 2).message.contains("actual=2"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentsCountPattern includes the expected pattern in its message") {
    assertEquals(
      FailureReason.InvalidArgumentsCountPattern("foo", 2).message.contains("foo"),
      true,
    )
  }

  test("FailureReason.InvalidArgumentsCountPattern includes the actual count in its message") {
    assertEquals(
      FailureReason.InvalidArgumentsCountPattern("foo", 2).message.contains("actual=2"),
      true,
    )
  }

  test("FailureReason.DivisionByZero includes the left expression in its message") {
    assertEquals(
      FailureReason.DivisionByZero(left, right).message.contains(left.show),
      true,
    )
  }

  test("FailureReason.DivisionByZero includes the right expression in its message") {
    assertEquals(
      FailureReason.DivisionByZero(left, right).message.contains(right.show),
      true,
    )
  }

  test("InvalidArgumentsCount.atLeast allows to create an instance with a minimum count") {
    assertEquals(
      FailureReason.InvalidArgumentsCount.atLeast(1, 2).message.contains("min=1"),
      true,
    )
  }

  test("InvalidArgumentsCount.atMost allows to create an instance with a maximum count") {
    assertEquals(
      FailureReason.InvalidArgumentsCount.atMost(1, 2).message.contains("max=1"),
      true,
    )
  }

  test("InvalidArgumentsCount.exactly allows to create an instance with an exact count") {
    assertEquals(
      FailureReason.InvalidArgumentsCount.exactly(1, 2).message.contains("min=1"),
      true,
    )
  }

}
