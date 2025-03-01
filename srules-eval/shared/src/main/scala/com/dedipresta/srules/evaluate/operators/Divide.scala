package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Divide:

  def apply[Ctx](): Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =

        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.withExactly2(op))
          .flatMap {
            case (Expr.RInt(a), Expr.RInt(b))       => Either.catchNonFatal(Expr.RInt(a / b))
            case (Expr.RLong(a), Expr.RLong(b))     => Either.catchNonFatal(Expr.RLong(a / b))
            case (Expr.RFloat(a), Expr.RFloat(b))   => Either.catchNonFatal(Expr.RFloat(a / b))
            case (Expr.RDouble(a), Expr.RDouble(b)) => Either.catchNonFatal(Expr.RDouble(a / b))
            case (l, r)                             => Left(FailureReason.InvalidArgumentTypes("Numeric", List(l, r))).opError(op, args)
          }
          .leftMap {
            case e: EvaluationError => e
            case _                  => EvaluationError.DivisionByZero(op, args)
          }
