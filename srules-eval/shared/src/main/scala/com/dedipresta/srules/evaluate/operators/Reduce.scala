package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Reduce:

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
                ls   <- evaluator.evaluatedToList(op, expr, ctx)
                data <- ls.traverse(evaluator.deepEvaluateFunctions(_, ctx))
                head <- data.headOption.toRight(FailureReason.InvalidArgumentValue("Non-empty list", data.toExpr)).opError(op, args)
                acc   = ctx.withAccumulator(index = 0, current = head, acc = head) // not correct but temporary
                res  <- data.tail.zipWithIndex
                          .foldLeft(acc.asRight[EvaluationError]) { case (eitherAcc, (expr, index)) =>
                            eitherAcc.flatMap { (acc: RuleCtx.WithAccumulator[Ctx]) =>
                              val nextAcc = acc.withAccumulator(index + 1, current = expr, acc = acc.acc)
                              evaluator.evaluate(fn, nextAcc).map(newAccValue => nextAcc.copy(acc = newAccValue))
                            }
                          }
                          .map(_.acc)
              } yield res
            case (_, other)                 => Left(FailureReason.InvalidArgumentType("Function", other)).opError(op, args)
          }
