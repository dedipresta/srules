package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Concat:

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
          .flatMap(_.withAtLeast1[F](op))
          .flatMap {
            case (Expr.RString(s1), tail) => tail.strings.bimap(_.opError(op, args), ls => (s1 :: ls).mkString.toExpr).liftTo[F]
            case (Expr.RList(ls1), tail)  => tail.lists.bimap(_.opError(op, args), ls => (ls1 :: ls).flatten.toExpr).liftTo[F]
            case (other1, other2)         => F.raiseError(FailureReason.InvalidArgumentTypes("List or String", other1 :: other2).opError(op, args))
          }
