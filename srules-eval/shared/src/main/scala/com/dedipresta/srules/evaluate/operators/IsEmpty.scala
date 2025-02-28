package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object IsEmpty:

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
          .flatMap {
            case Expr.RString(s) => Right(s.isEmpty.toExpr)
            case Expr.RList(l)   => Right(l.isEmpty.toExpr)
            case other           => EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("String or List", other)).asLeft
          }
