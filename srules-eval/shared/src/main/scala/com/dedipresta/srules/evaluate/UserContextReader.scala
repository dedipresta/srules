package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

import cats.syntax.all.*

trait UserContextReader[Ctx]:
  def read(ctx: Ctx, name: String, defaultValue: Option[Expr]): Either[EvaluationError, Expr]

object UserContextReader:

  def forMapAny(notFoundToNull: Boolean): UserContextReader[Map[String, Any]] =
    new UserContextReader[Map[String, Any]]:
      def read(ctx: Map[String, Any], name: String, defaultValue: Option[Expr]): Either[EvaluationError, Expr] =
        ctx.get(name) match {
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
          case None        =>
            defaultValue match {
              case Some(v) => v.asRight
              case None    => if (notFoundToNull) Expr.RNull.asRight else Left(EvaluationError.VariableNotFound(name))
            }
        }
