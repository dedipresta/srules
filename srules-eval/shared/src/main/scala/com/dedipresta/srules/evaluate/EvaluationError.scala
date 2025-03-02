package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr
import com.dedipresta.srules.given

import cats.syntax.all.*

sealed trait EvaluationError:
  def message: String
object EvaluationError:
  case class OperatorNotFound(op: String, expr: Expr)                              extends EvaluationError:
    def message: String = s"Operator not found: op=$op, expr=${expr.show}"
  case class OperationFailure(op: String, args: List[Expr], reason: FailureReason) extends EvaluationError:
    def message: String = s"Operation failure: op=$op, args=[${args.map(_.show).mkString(",")}], reason=(${reason.message})"

  case class EvaluationException(error: EvaluationError) extends Exception(error.message)
  extension (err: EvaluationError) {
    def toException: EvaluationException = EvaluationException(err)
  }

trait FailureReason:
  def message: String
object FailureReason:
  case class Message(message: String)                                               extends FailureReason
  case class InvalidArgumentType(expected: String, actual: Expr)                    extends FailureReason:
    def message: String = s"Invalid argument type: expected=$expected, actual=${actual.show}"
  case class InvalidArgumentTypes(expected: String, actual: List[Expr])             extends FailureReason:
    def message: String = s"Invalid argument types: expected=$expected, actual=[${actual.map(_.show).mkString(",")}]"
  case class InvalidArgumentValue(expected: String, actual: Expr)                   extends FailureReason:
    def message: String = s"Invalid argument value: expected=$expected, actual=${actual.show}"
  case class InvalidArgumentsCount(min: Option[Int], max: Option[Int], actual: Int) extends FailureReason:
    def message: String =
      val minStr = min.fold("")(m => s"min=$m")
      val maxStr = max.fold("")(m => s"max=$m")
      s"Invalid arguments count: $minStr $maxStr, actual=$actual"
  object InvalidArgumentsCount {
    def atLeast(min: Int, actual: Int): InvalidArgumentsCount = InvalidArgumentsCount(Some(min), None, actual)
    def atMost(max: Int, actual: Int): InvalidArgumentsCount  = InvalidArgumentsCount(None, Some(max), actual)
    def exactly(n: Int, actual: Int): InvalidArgumentsCount   = InvalidArgumentsCount(Some(n), Some(n), actual)
  }
  case class InvalidArgumentsCountPattern(expected: String, actual: Int)            extends FailureReason:
    def message: String = s"Invalid arguments count pattern: expected=$expected, actual=$actual"
  case class DivisionByZero(left: Expr, right: Expr)                                extends FailureReason:
    def message: String = s"Division by zero: left=${left.show}, right=${right.show}"
  case class VariableNotFound(name: String)                                         extends FailureReason:
    def message: String = s"Variable not found: name=$name"
