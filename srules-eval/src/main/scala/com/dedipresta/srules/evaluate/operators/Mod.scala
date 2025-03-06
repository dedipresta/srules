package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Mod:

  def apply[F[_], Ctx](
      sameTypeOnly: Boolean = false,
  )(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] = new Operator[F, Ctx, EvaluationError]:
    def evaluate(
        evaluator: ExprEvaluator[F, Ctx, EvaluationError],
        op: String,
        args: List[Expr],
        ctx: RuleCtx[Ctx],
    ): F[Expr] =
      args
        .traverse(evaluator.deepEvaluateFunctions(_, ctx))
        .flatMap(_.withExactly2[F](op))
        .flatMap(modFn(_, _).leftMap(_.opError(op, args)).liftTo[F])

    private val modWithSameTypeOnly: (Expr, Expr) => Either[FailureReason, Expr] =
      // same type
      case (Expr.RInt(a), Expr.RInt(b))       => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RLong(a), Expr.RLong(b))     => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RFloat(a), Expr.RFloat(b))   => (a % b).toExpr.asRight
      case (Expr.RDouble(a), Expr.RDouble(b)) => (a % b).toExpr.asRight
      case (l, r)                             => FailureReason.InvalidArgumentTypes("Numeric", List(l, r)).asLeft

    private val modWithDifferentTypesAllowed: (Expr, Expr) => Either[FailureReason, Expr] =
      // same type
      case (Expr.RInt(a), Expr.RInt(b))       => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RLong(a), Expr.RLong(b))     => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RFloat(a), Expr.RFloat(b))   => (a % b).toExpr.asRight
      case (Expr.RDouble(a), Expr.RDouble(b)) => (a % b).toExpr.asRight
      // different types
      case (Expr.RInt(a), Expr.RLong(b))      => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RInt(a), Expr.RFloat(b))     => (a % b).toExpr.asRight
      case (Expr.RInt(a), Expr.RDouble(b))    => (a % b).toExpr.asRight
      case (Expr.RLong(a), Expr.RInt(b))      => Either.catchNonFatal(a % b).bimap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr), _.toExpr)
      case (Expr.RLong(a), Expr.RFloat(b))    => (a % b).toExpr.asRight
      case (Expr.RLong(a), Expr.RDouble(b))   => (a % b).toExpr.asRight
      case (Expr.RFloat(a), Expr.RInt(b))     => (a % b).toExpr.asRight
      case (Expr.RFloat(a), Expr.RLong(b))    => (a % b).toExpr.asRight
      case (Expr.RFloat(a), Expr.RDouble(b))  => (a % b).toExpr.asRight
      case (Expr.RDouble(a), Expr.RInt(b))    => (a % b).toExpr.asRight
      case (Expr.RDouble(a), Expr.RLong(b))   => (a % b).toExpr.asRight
      case (Expr.RDouble(a), Expr.RFloat(b))  => (a % b).toExpr.asRight
      case (l, r)                             => FailureReason.InvalidArgumentTypes("Numeric", List(l, r)).asLeft

    private val modFn: (Expr, Expr) => Either[FailureReason, Expr] =
      if sameTypeOnly then modWithSameTypeOnly else modWithDifferentTypesAllowed
