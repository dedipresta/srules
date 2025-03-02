package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

import cats.syntax.all.*

trait UserContextReader[Ctx]:
  def read(ctx: Ctx, name: String, defaultValue: Option[Expr]): Either[FailureReason, Expr]

object UserContextReader:

  def forMapExpr(notFoundToNull: Boolean): UserContextReader[Map[String, Expr]] =
    new UserContextReader[Map[String, Expr]]:
      def read(ctx: Map[String, Expr], name: String, defaultValue: Option[Expr]): Either[FailureReason, Expr] =
        ctx.get(name) match {
          case Some(value) => value.asRight
          case None        =>
            defaultValue match {
              case Some(v) => v.asRight
              case None    => if (notFoundToNull) Expr.RNull.asRight else Left(FailureReason.VariableNotFound(name))
            }
        }

  def noContext[Unit](notFoundToNull: Boolean): UserContextReader[Unit] =
    new UserContextReader[Unit]:
      def read(ctx: Unit, name: String, defaultValue: Option[Expr]): Either[FailureReason, Expr] =
        defaultValue match {
          case Some(v) => v.asRight
          case None    => if (notFoundToNull) Expr.RNull.asRight else Left(FailureReason.VariableNotFound(name))
        }
