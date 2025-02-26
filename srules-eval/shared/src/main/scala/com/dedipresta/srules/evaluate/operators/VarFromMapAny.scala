package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

// this operator requires a class because it directly accesses the context
final class VarFromMapAny[RCtx] extends Operator[Map[String, Any], EvaluationError]:

  private val ArrayIndex  = "__index__"
  private val ArrayValue  = "__value__"
  private val Accumulator = "__acc__"

  def evaluate(
      evaluator: ExprEvaluator[Map[String, Any], EvaluationError],
      op: String,
      args: List[Expr],
      ctx: RuleCtx[Map[String, Any]],
  ): Either[EvaluationError, Expr] =
    args
      .traverse(evaluator.evaluate(_, ctx))
      .flatMap(_.withExactly1(op))
      .flatMap(_.mapString(op, identity))
      .flatMap { name =>
        name match {
          case ArrayIndex  => ctx.indexValue.toRight(EvaluationError.VariableNotFound(name)).map(_.toExpr)
          case ArrayValue  => ctx.currentValue.toRight(EvaluationError.VariableNotFound(name))
          case Accumulator => ctx.accumulatorValue.toRight(EvaluationError.VariableNotFound(name))
          case other       =>
            ctx.userCtx.get(other) match {
              case Some(value) =>
                value match {
                  case v: Int     => Right(Expr.RInt(v))
                  case v: Long    => Right(Expr.RLong(v))
                  case v: Float   => Right(Expr.RFloat(v))
                  case v: Double  => Right(Expr.RDouble(v))
                  case v: String  => Right(Expr.RString(v))
                  case v: Boolean => Right(Expr.RBoolean(v))
                  case v          => Left(EvaluationError.UnsupportedVariableType(name))
                }
              case None        => Left(EvaluationError.VariableNotFound(name))
            }
        }
      }
