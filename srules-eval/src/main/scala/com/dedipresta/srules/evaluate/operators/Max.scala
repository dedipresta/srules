package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Max:

  def apply[F[_], Ctx](
      sameTypeOnly: Boolean = false,
  )(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.withAtLeast1[F](op))
          .flatMap(maxFn(_, _).leftMap(_.opError(op, args)).liftTo[F])

      private val maxWithSameTypeOnly: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        case (Expr.RInt(a), tail)    => tail.foldExtract(a)(_ max _).map(_.toExpr)
        case (Expr.RLong(a), tail)   => tail.foldExtract(a)(_ max _).map(_.toExpr)
        case (Expr.RFloat(a), tail)  => tail.foldExtract(a)(_ max _).map(_.toExpr)
        case (Expr.RDouble(a), tail) => tail.foldExtract(a)(_ max _).map(_.toExpr)
        case (Expr.RString(a), tail) => tail.foldExtract(a)(_ max _).map(_.toExpr)
        case (other, _)              => Left(FailureReason.InvalidArgumentType("Numeric", other))

      private val maxWithDifferentTypesAllowedFold: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        (head, tail) => tail.foldLeftM(head)((acc, expr) => maxWithDifferentTypesAllowed(acc, expr))

      private val maxWithDifferentTypesAllowed: (Expr, Expr) => Either[FailureReason, Expr] =
        // same type
        case (Expr.RInt(a), Expr.RInt(b))       => a.max(b).toExpr.asRight
        case (Expr.RLong(a), Expr.RLong(b))     => a.max(b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RFloat(b))   => a.max(b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RDouble(b)) => a.max(b).toExpr.asRight
        case (Expr.RString(a), Expr.RString(b)) => a.max(b).toExpr.asRight
        // different types
        case (Expr.RInt(a), Expr.RLong(b))      => a.toLong.max(b).toExpr.asRight
        case (Expr.RInt(a), Expr.RFloat(b))     => a.toFloat.max(b).toExpr.asRight
        case (Expr.RInt(a), Expr.RDouble(b))    => a.toDouble.max(b).toExpr.asRight
        case (Expr.RLong(a), Expr.RInt(b))      => a.max(b.toLong).toExpr.asRight
        case (Expr.RLong(a), Expr.RFloat(b))    => a.toFloat.max(b).toExpr.asRight
        case (Expr.RLong(a), Expr.RDouble(b))   => a.toDouble.max(b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RInt(b))     => a.max(b.toFloat).toExpr.asRight
        case (Expr.RFloat(a), Expr.RLong(b))    => a.max(b.toFloat).toExpr.asRight
        case (Expr.RFloat(a), Expr.RDouble(b))  => a.toDouble.max(b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RInt(b))    => a.max(b.toDouble).toExpr.asRight
        case (Expr.RDouble(a), Expr.RLong(b))   => a.max(b.toDouble).toExpr.asRight
        case (Expr.RDouble(a), Expr.RFloat(b))  => a.max(b.toDouble).toExpr.asRight
        case (l, r)                             => FailureReason.InvalidArgumentTypes("Numeric or String", List(l, r)).asLeft

      private val maxFn: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        if sameTypeOnly then maxWithSameTypeOnly else maxWithDifferentTypesAllowedFold
