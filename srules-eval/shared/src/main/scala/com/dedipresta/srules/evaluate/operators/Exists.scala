package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Exists:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .withExactly2(op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluatedToList(op, expr, ctx)
                res  <- data.zipWithIndex
                          .foldLeft[Either[EvaluationError, Boolean]](false.asRight) { case (acc, (expr, index)) =>
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

            case (_, other) => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }
