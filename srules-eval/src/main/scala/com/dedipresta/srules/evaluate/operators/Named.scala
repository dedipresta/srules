package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Named:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        if (args.length == 1) // single argument is a variable name
          args
            .withExactly1[F](op)
            .flatMap(evaluator.evaluatedToString(op, _, ctx).map(_.toNamedVar))
            .flatMap(evaluator.deepEvaluateFunctions(_, ctx))
        else
          args
            .withExactly3[F](op)
            .flatMap { case (nameExpr, namedExpr, expr) =>
              for {
                name  <- evaluator.evaluatedToString(op, nameExpr, ctx)
                value <- evaluator.deepEvaluateFunctions(expr, ctx.addNamedValue(name, namedExpr)) // namedExpr given not evaluated
              } yield value

            }
