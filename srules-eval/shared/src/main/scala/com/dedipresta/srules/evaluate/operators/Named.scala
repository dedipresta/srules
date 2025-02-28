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
            .flatMap(evaluator.evaluate(_, ctx))
            .flatMap(_.mapString(_.toNamedVar).leftMap(_.opError(op, args)))
            .flatMap(evaluator.evaluate(_, ctx))
        else
          args
            .withExactly3(op)
            .flatMap { case (nameExpr, namedExpr, expr) =>
              println(s"Named: nameExpr: $nameExpr, namedExpr: $namedExpr, expr: $expr")
              for {
                name  <- evaluator.evaluate(nameExpr, ctx).flatMap(_.withString.leftMap(_.opError(op, args)))
                named <- evaluator.evaluate(namedExpr, ctx)
                value <- evaluator.evaluate(expr, ctx.addNamedValue(name, named))
              } yield value

            }
