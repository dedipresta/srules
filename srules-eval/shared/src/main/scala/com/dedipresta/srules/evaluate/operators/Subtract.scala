package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Subtract:

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
          .flatMap(_.withAtLeast1[F](op))
          .flatMap { (evaluated, tail) =>
            if (tail.isEmpty)
              evaluated match
                // Unary minus operator (value obtained after evaluating the expression)
                case Expr.RInt(a)    => Expr.RInt(-a).pure[F]
                case Expr.RLong(a)   => Expr.RLong(-a).pure[F]
                case Expr.RFloat(a)  => Expr.RFloat(-a).pure[F]
                case Expr.RDouble(a) => Expr.RDouble(-a).pure[F]
                case other           => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, args))
            else
              evaluated match
                case Expr.RInt(a)    => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
                case Expr.RLong(a)   => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
                case Expr.RFloat(a)  => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
                case Expr.RDouble(a) => tail.foldExtract(a)(_ - _).bimap(_.opError(op, args), _.toExpr).liftTo[F]
                case other           => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, args))
          }
