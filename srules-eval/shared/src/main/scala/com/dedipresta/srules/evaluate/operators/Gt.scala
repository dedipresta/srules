package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

import scala.math.Ordering.Implicits.*

object Gt:
  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:

      private def f[N: Numeric](i: N, j: N): (N, Boolean) = j -> (i > j)

      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =

        args.headOption.traverse(evaluator.evaluate(_, ctx)).flatMap { // evaluate head only to allow for short-circuiting
          case None                => Right(Expr.RBoolean(true)) // true by vacuous truth
          case Some(evaluatedHead) =>
            evaluatedHead match {
              case Expr.RInt(i)    => args.tail.foldEvaluateExtractWhile(evaluator, ctx)(op, i)(f).map(r => Expr.RBoolean(r._2))
              case Expr.RLong(i)   => args.tail.foldEvaluateExtractWhile(evaluator, ctx)(op, i)(f).map(r => Expr.RBoolean(r._2))
              case Expr.RFloat(i)  => args.tail.foldEvaluateExtractWhile(evaluator, ctx)(op, i)(f).map(r => Expr.RBoolean(r._2))
              case Expr.RDouble(i) => args.tail.foldEvaluateExtractWhile(evaluator, ctx)(op, i)(f).map(r => Expr.RBoolean(r._2))
              case _               => Left(EvaluationError.InvalidArgumentType(op, args))
            }
        }
