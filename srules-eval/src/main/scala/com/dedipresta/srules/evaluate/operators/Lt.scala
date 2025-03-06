package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

import scala.math.Ordering.Implicits.*

object Lt:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      private def f[N: Numeric](i: N, j: N): (N, Boolean) = j -> (i < j)

      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args.headOption.traverse(evaluator.deepEvaluateFunctions(_, ctx)).flatMap { // evaluate head only to allow for short-circuiting
          case None                => Expr.RBoolean(true).pure[F] // true by vacuous truth
          case Some(evaluatedHead) =>
            evaluatedHead match {
              case Expr.RInt(i)    => args.tail.foldDeepEvaluateWhile(evaluator, op, ctx)(i)(f).map(_._2.toExpr)
              case Expr.RLong(i)   => args.tail.foldDeepEvaluateWhile(evaluator, op, ctx)(i)(f).map(_._2.toExpr)
              case Expr.RFloat(i)  => args.tail.foldDeepEvaluateWhile(evaluator, op, ctx)(i)(f).map(_._2.toExpr)
              case Expr.RDouble(i) => args.tail.foldDeepEvaluateWhile(evaluator, op, ctx)(i)(f).map(_._2.toExpr)
              case other           => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, args))
            }
        }
