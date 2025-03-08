package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Exists:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .withExactly2[F](op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluatedToList(op, expr, ctx)
                res  <- data.zipWithIndex
                          .foldLeft[F[Boolean]](false.pure[F]) { case (acc, (expr, index)) =>
                            acc.flatMap { (found: Boolean) =>
                              if (found)
                                acc
                              else
                                for {
                                  v   <- evaluator.deepEvaluateFunctions(expr, ctx)
                                  res <- evaluator.evaluatedToBoolean(op, fn, ctx.withIndexedValue(index, v))
                                } yield res
                            }
                          }
              } yield res.toExpr

            case (_, other) => F.raiseError(FailureReason.InvalidArgumentType("Function", other).opError(op, args))
          }
