package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Filter:

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
                ls   <- evaluator.evaluate(expr, ctx).flatMap(_.withList.leftMap(EvaluationError.OperationFailure(op, args, _)))
                data <- ls.traverse(evaluator.evaluate(_, ctx)) // evaluate inner list elements, no short-circuit so we can do a traverse
                res  <- filter(evaluator, op, args, ctx, fn, data)
              } yield res
            case (_, other)                 => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }

      private def filter(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
          fn: Expr.RFunction,
          data: List[Expr],
      ): Either[EvaluationError, Expr.RList] =
        data.zipWithIndex
          .flatTraverse { (el, i) =>
            evaluator
              .evaluate(fn, ctx.withIndexedValue(i, el))
              .flatMap(_.mapBoolean(b => Option.when(b)(el).toList).leftMap(EvaluationError.OperationFailure(op, args, _)))
          }
          .map(_.toExpr)
