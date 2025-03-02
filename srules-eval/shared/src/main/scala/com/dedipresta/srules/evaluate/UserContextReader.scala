package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

trait UserContextReader[F[_], Ctx]:
  def read(ctx: Ctx, name: String, defaultValue: Option[Expr]): F[Expr]

object UserContextReader:

  def forMapExpr[F[_]](notFoundToNull: Boolean)(using F: MonadError[F, EvaluationError]): UserContextReader[F, Map[String, Expr]] =
    new UserContextReader[F, Map[String, Expr]]:
      def read(ctx: Map[String, Expr], name: String, defaultValue: Option[Expr]): F[Expr] =
        ctx.get(name) match {
          case Some(value) => value.pure[F]
          case None        => handleNotFound(name, defaultValue, notFoundToNull)
        }

  def noContext[F[_]](notFoundToNull: Boolean)(using F: MonadError[F, EvaluationError]): UserContextReader[F, Unit] =
    new UserContextReader[F, Unit]:
      def read(ctx: Unit, name: String, defaultValue: Option[Expr]): F[Expr] =
        handleNotFound(name, defaultValue, notFoundToNull)

  private def handleNotFound[F[_]](name: String, defaultValue: Option[Expr], notFoundToNull: Boolean)(using
      F: MonadError[F, EvaluationError],
  ): F[Expr] =
    defaultValue match
      case Some(v) => v.pure[F]
      case None    => if (notFoundToNull) Expr.RNull.pure[F] else F.raiseError(FailureReason.VariableNotFound(name).opError("readUserContext", Nil))
