package com.dedipresta.srules.evaluate.operators

import cats.syntax.all.*
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

object Exists:
  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args.asRight
          .flatMap(_.exactly(1, op).as(args.head))
          .flatMap {
            case f @ Expr.RFunction("var", _) =>
              evaluator
                .evaluate(f, ctx)
                .as(Expr.RBoolean(true))
                .leftFlatMap {
                  case _: EvaluationError.VariableNotFound => Right(Expr.RBoolean(false))
                  case e                                   => Left(e)
                }
            case _                            => Left(EvaluationError.InvalidArgumentType(op, args))

          }
