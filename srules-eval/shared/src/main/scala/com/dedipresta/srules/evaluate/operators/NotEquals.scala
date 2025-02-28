package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

// check if all elements are distinct
object NotEquals:

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
          .flatMap {
            case e1 :: e2 :: Nil => Right(Expr.RBoolean(e1 != e2)) // most common case
            case Nil             => Right(Expr.RBoolean(true))     // true by vacuous truth
            case head :: Nil     => Right(Expr.RBoolean(true))     // true by vacuous truth
            case other           => Right(Expr.RBoolean(other.distinct.length == other.length))
          }
