package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

// check if all elements are distinct
object NotEquals:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap {
            case e1 :: e2 :: Nil => Expr.RBoolean(e1 != e2).pure[F] // most common case
            case Nil             => Expr.RBoolean(true).pure[F]     // true by vacuous truth
            case head :: Nil     => Expr.RBoolean(true).pure[F]     // true by vacuous truth
            case other           => Expr.RBoolean(other.distinct.length == other.length).pure[F]
          }
