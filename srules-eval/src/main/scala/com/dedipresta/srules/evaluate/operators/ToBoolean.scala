package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ToBoolean:

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
          .flatMap(_.withExactly1[F](op))
          .flatMap(toBooleanFn(_).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private val toBooleanFn: Expr => Either[FailureReason, Boolean] =
        case Expr.RString(s)  => s.toBooleanOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Boolean", s.toExpr))
        case Expr.RInt(i)     => Right(i != 0)
        case Expr.RBoolean(b) => Right(b)
        case Expr.RLong(l)    => Right(l != 0L)
        case Expr.RDouble(d)  => Right(d != 0.0)
        case Expr.RFloat(f)   => Right(f != 0.0f)
        case other            => Left(FailureReason.InvalidArgumentType("Cannot be cast to Boolean", other))
