package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Divide:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
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
          .flatMap {
            case (Expr.RInt(a), Expr.RInt(b))       =>
              Either.catchNonFatal(Expr.RInt(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr)).opError(op, args).liftTo[F]
            case (Expr.RLong(a), Expr.RLong(b))     =>
              Either.catchNonFatal(Expr.RLong(a / b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr)).opError(op, args).liftTo[F]
            case (Expr.RFloat(a), Expr.RFloat(b))   => Expr.RFloat(a / b).pure[F]
            case (Expr.RDouble(a), Expr.RDouble(b)) => Expr.RDouble(a / b).pure[F]
            case (l, r)                             => F.raiseError(FailureReason.InvalidArgumentTypes("Numeric", List(l, r)).opError(op, args))
          }
