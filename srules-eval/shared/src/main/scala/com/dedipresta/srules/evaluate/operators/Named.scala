package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Named:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        if (args.length == 1) // single argument is a variable name
          args
            .withExactly1(op)
            .flatMap(evaluator.evaluatedToString(op, _, ctx).map(_.toNamedVar))
            .flatMap(evaluator.deepEvaluateFunctions(_, ctx))
        else
          args
            .withExactly3(op)
            .flatMap { case (nameExpr, namedExpr, expr) =>
              for {
                name  <- evaluator.evaluatedToString(op, nameExpr, ctx)
                value <- evaluator.deepEvaluateFunctions(expr, ctx.addNamedValue(name, namedExpr)) // namedExpr given not evaluated
              } yield value

            }
