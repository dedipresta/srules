package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Mod:

  def apply[Ctx](): Operator[Ctx, EvaluationError] = new Operator[Ctx, EvaluationError]:
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
          case (Expr.RInt(a), Expr.RInt(b))       =>
            Either.catchNonFatal(Expr.RInt(a % b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr)).opError(op, args)
          case (Expr.RLong(a), Expr.RLong(b))     =>
            Either.catchNonFatal(Expr.RLong(a % b)).leftMap(_ => FailureReason.DivisionByZero(a.toExpr, b.toExpr)).opError(op, args)
          case (Expr.RFloat(a), Expr.RFloat(b))   => Expr.RFloat(a % b).asRight
          case (Expr.RDouble(a), Expr.RDouble(b)) => Expr.RDouble(a % b).asRight
          case (l, r)                             => Left(FailureReason.InvalidArgumentTypes("Numeric", List(l, r))).opError(op, args)
        }
