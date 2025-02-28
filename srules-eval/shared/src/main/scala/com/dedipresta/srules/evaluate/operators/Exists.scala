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
        args // FIXME OK ?
          .withExactly2(op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluate(expr, ctx).flatMap(_.withList.leftMap(EvaluationError.OperationFailure(op, args, _)))
                res  <- data.zipWithIndex
                          .foldLeft[Either[EvaluationError, Boolean]](false.asRight) { case (acc, (expr, index)) =>
                            acc.flatMap { (found: Boolean) =>
                              if (found)
                                acc
                              else
                                for {
                                  evaluated <- evaluator.evaluate(expr, ctx)
                                  expr      <- evaluator.evaluate(fn, ctx.withIndexedValue(index, evaluated))
                                  b         <- expr.withBoolean.leftMap(EvaluationError.OperationFailure(op, args, _))
                                } yield b
                            }
                          }
              } yield res.toExpr

            case (_, other) => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }
