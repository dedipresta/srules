package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object MapFn:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(evaluator: ExprEvaluator[Ctx, EvaluationError], op: String, args: List[Expr], ctx: RuleCtx[Ctx]): Either[EvaluationError, Expr] =
        // first argument is a list, second argument is a function
        // but we cannot evaluate the function now since it may access data from the list
        // the list can also not be a list yet but a reference to a variable, ...
        args
          .withExactly2(op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluatedToList(op, expr, ctx)
                res  <- data.zipWithIndex
                          .foldLeft[Either[EvaluationError, Vector[Expr]]](Vector.empty.asRight) { case (acc, (expr, index)) =>
                            acc.flatMap { (accValue: Vector[Expr]) =>
                              for {
                                evaluated <- evaluator.deepEvaluateFunctions(expr, ctx)
                                expr      <- evaluator.evaluate(fn, ctx.withIndexedValue(index, evaluated))
                              } yield accValue.appended(expr)
                            }
                          }
              } yield res.toList.toExpr
            case (_, other)                 => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }
