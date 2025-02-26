package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

// equal 2 by 2 so true for empty list, single element list, and list with all elements equal
object Equal:
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
          .flatMap { evaluated =>
            evaluated.headOption match {
              case Some(head) => Right(Expr.RBoolean(evaluated.forall(_ == head)))
              case None       => Right(Expr.RBoolean(true)) // true by vacuous truth
            }
          }
