package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Divide:

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
          .flatMap(_.withExactly2[F](op))
          .flatMap(divFn(_, _).leftMap(_.opError(op, args)).liftTo[F])

      private val divideSameTypeOnly: (Expr, Expr) => Either[FailureReason, Expr] =
        case (Expr.RInt(a), Expr.RInt(b))       =>
          Either.catchNonFatal(Expr.RInt(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RLong(a), Expr.RLong(b))     =>
          Either.catchNonFatal(Expr.RLong(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RFloat(a), Expr.RFloat(b))   => Expr.RFloat(a / b).asRight
        case (Expr.RDouble(a), Expr.RDouble(b)) => Expr.RDouble(a / b).asRight
        case (l, r)                             => Left(FailureReason.InvalidArgumentTypes("Numeric", List(l, r)))

      private val divideWithDifferentTypesAllowed: (Expr, Expr) => Either[FailureReason, Expr] =
        // same types
        case (Expr.RInt(a), Expr.RInt(b))       =>
          Either.catchNonFatal(Expr.RInt(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RLong(a), Expr.RLong(b))     =>
          Either.catchNonFatal(Expr.RLong(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RFloat(a), Expr.RFloat(b))   => Expr.RFloat(a / b).asRight
        case (Expr.RDouble(a), Expr.RDouble(b)) => Expr.RDouble(a / b).asRight
        // different types
        case (Expr.RInt(a), Expr.RLong(b))      => Either.catchNonFatal(Expr.RLong(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RInt(a), Expr.RFloat(b))     => Expr.RFloat(a / b).asRight
        case (Expr.RInt(a), Expr.RDouble(b))    => Expr.RDouble(a / b).asRight
        case (Expr.RLong(a), Expr.RInt(b))      => Either.catchNonFatal(Expr.RLong(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr))
        case (Expr.RLong(a), Expr.RFloat(b))    => Expr.RFloat(a / b).asRight
        case (Expr.RLong(a), Expr.RDouble(b))   => Expr.RDouble(a / b).asRight
        case (Expr.RFloat(a), Expr.RInt(b))     => Expr.RFloat(a / b).asRight
        case (Expr.RFloat(a), Expr.RLong(b))    => Expr.RFloat(a / b).asRight
        case (Expr.RFloat(a), Expr.RDouble(b))  => Expr.RDouble(a / b).asRight
        case (Expr.RDouble(a), Expr.RInt(b))    => Expr.RDouble(a / b).asRight
        case (Expr.RDouble(a), Expr.RLong(b))   => Expr.RDouble(a / b).asRight
        case (Expr.RDouble(a), Expr.RFloat(b))  => Expr.RDouble(a / b).asRight
        case (l, r)                             => Left(FailureReason.InvalidArgumentTypes("Numeric", List(l, r)))

      private val divFn: (Expr, Expr) => Either[FailureReason, Expr] =
        if (sameTypeOnly) divideSameTypeOnly else divideWithDifferentTypesAllowed
