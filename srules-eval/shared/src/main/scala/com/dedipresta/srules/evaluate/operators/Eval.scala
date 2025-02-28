package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Eval:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.evaluate(_, ctx))
          .flatMap(_.withExactly1(op))
          .flatMap(subEval(evaluator, op, _, ctx))

      private def subEval(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          arg: Expr,
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        evaluator.evaluate(arg, ctx).flatMap {
          case Expr.RList(ls)    => ls.traverse(subEval(evaluator, op, _, ctx)).map(Expr.RList.apply)
          case f: Expr.RFunction => evaluator.evaluate(f, ctx).flatMap(subEval(evaluator, op, _, ctx))
          case other             => other.asRight
        }
