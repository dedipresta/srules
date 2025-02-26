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
                ls   <- evaluator.evaluate(expr, ctx).flatMap(_.mapList[List[Expr]](op, identity))
                data <- ls.traverse(evaluator.evaluate(_, ctx)) // evaluate inner list elements
                res  <- data.zipWithIndex.traverse((el, i) => evaluator.evaluate(fn, ctx.withIndexedValue(i, el))).map(_.toExpr)
              } yield res
            case _                          => Left(EvaluationError.InvalidArgumentType(op, args))
          }
