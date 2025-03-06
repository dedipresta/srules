package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object ToInt:

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
          .flatMap(toIntFn(_).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private val toIntFn: Expr => Either[FailureReason, Int] =
        case Expr.RString(s)  => s.toIntOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Int", s.toExpr))
        case Expr.RInt(i)     => Right(i)
        case Expr.RBoolean(b) => Right(if (b) 1 else 0)
        case Expr.RLong(l)    => Right(l.toInt)
        case Expr.RDouble(d)  => Right(d.toInt)
        case Expr.RFloat(f)   => Right(f.toInt)
        case other            => Left(FailureReason.InvalidArgumentType("Cannot be cast to Int", other))
