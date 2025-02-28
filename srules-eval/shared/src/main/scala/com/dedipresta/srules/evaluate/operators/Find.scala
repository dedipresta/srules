package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Find:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        // first argument is a list, second argument is a function
        // but we cannot evaluate the function now since it may access data from the list
        // the list can also not be a list yet but a reference to a variable, ...
        args
          .withExactly2(op)
          .flatMap {
            case (expr, fn: Expr.RFunction) =>
              for {
                data <- evaluator.evaluate(expr, ctx).flatMap(_.withList.leftMap(EvaluationError.OperationFailure(op, args, _)))
                res  <- data.zipWithIndex
                          .foldLeft[Either[EvaluationError, Option[Expr]]](Option.empty[Expr].asRight) { case (acc, (expr, index)) =>
                            acc.flatMap { (optValue: Option[Expr]) =>
                              optValue match {
                                case Some(_) => acc
                                case None    =>
                                  for {
                                    evaluated <- evaluator.evaluate(expr, ctx)
                                    result    <- evaluator.evaluate(fn, ctx.withIndexedValue(index, evaluated))
                                    asBool    <- result.withBoolean.leftMap(EvaluationError.OperationFailure(op, args, _))
                                  } yield if (asBool) Some(evaluated) else None
                              }
                            }
                          }
              } yield res.getOrElse(Expr.RNull)
            case (_, other)                 => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }
