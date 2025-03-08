package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

// convert an expression to a float value
object ToFloat:

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
          .flatMap(toFloatFn(_).bimap(_.opError(op, args), _.toExpr).liftTo[F])

      private val toFloatFn: Expr => Either[FailureReason, Float] =
        case Expr.RString(s)  => s.toFloatOption.toRight(FailureReason.InvalidArgumentValue("Cannot be cast to Float", s.toExpr))
        case Expr.RInt(i)     => Right(i.toFloat)
        case Expr.RBoolean(b) => Right(if (b) 1f else 0f)
        case Expr.RLong(l)    => Right(l.toFloat)
        case Expr.RDouble(d)  => Right(d.toFloat)
        case Expr.RFloat(f)   => Right(f)
        case other            => Left(FailureReason.InvalidArgumentType("Cannot be cast to Float", other))
