package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

// TODO improve error building
sealed trait EvaluationError
object EvaluationError:
  case class OperatorNotFound(op: String)                               extends EvaluationError
  case class InvalidArgument(op: String, args: List[Expr])              extends EvaluationError       // argument value
  sealed trait InvalidArgumentsCount                                    extends EvaluationError
  case class AtLeast(op: String, n: Int, args: List[Expr])              extends InvalidArgumentsCount
  case class AtMost(op: String, n: Int, args: List[Expr])               extends InvalidArgumentsCount
  case class Exactly(op: String, n: Int, args: List[Expr])              extends InvalidArgumentsCount
  case class ZZZZZ(op: String)                                          extends InvalidArgumentsCount // TODO FIXME
  case class DivisionByZero(op: String, args: List[Expr])               extends EvaluationError
  case class VariableNotFound(name: String)                             extends EvaluationError
  case class UnsupportedVariableType(name: String)                      extends EvaluationError
  case class OperatorRequiresNonEmptyList(op: String, args: List[Expr]) extends EvaluationError

  case class OperationFailure(op: String, args: List[Expr], reason: FailureReason) extends EvaluationError
  trait CustomError                                                                extends EvaluationError

trait FailureReason
object FailureReason:
  case class Message(msg: String)                                       extends FailureReason
  case class InvalidArgumentType(expected: String, actual: Expr)        extends FailureReason
  case class InvalidArgumentTypes(expected: String, actual: List[Expr]) extends FailureReason
  case class InvalidArgumentValue(actual: Expr)                         extends FailureReason
  case class InvalidArgumentCount()
