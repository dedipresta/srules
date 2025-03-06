package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ForAll:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        // first argument is a list, second argument is a function
        // but we cannot evaluate the function now since it may access data from the list
        // the list can also not be a list yet but a reference to a variable, ...
        args
          .withExactly2[F](op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluatedToList(op, expr, ctx)
                res  <- data.zipWithIndex
                          .foldLeft[F[Boolean]](true.pure[F]) { case (acc, (expr, index)) =>
                            acc.flatMap { (accValue: Boolean) =>
                              if (accValue)
                                for {
                                  evaluated <- evaluator.evaluate(expr, ctx)
                                  b         <- evaluator.evaluatedToBoolean(op, fn, ctx.withIndexedValue(index, evaluated))
                                } yield b
                              else acc
                            }
                          }
              } yield res.toExpr
            case (_, other)                 => F.raiseError(FailureReason.InvalidArgumentType("Function", other).opError(op, args))
          }
