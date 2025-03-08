package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Subtract:

  def apply[F[_], Ctx](
      sameTypeOnly: Boolean = false,
  )(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
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
          .flatMap { (evaluated, tail) =>
            if (tail.isEmpty)
              evaluated match
                // Unary minus operator (value obtained after evaluating the expression)
                case Expr.RInt(a)    => Expr.RInt(-a).pure[F]
                case Expr.RLong(a)   => Expr.RLong(-a).pure[F]
                case Expr.RFloat(a)  => Expr.RFloat(-a).pure[F]
                case Expr.RDouble(a) => Expr.RDouble(-a).pure[F]
                case other           => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, args))
            else if (sameTypeOnly) subWithSameTypeOnly(op)(evaluated, tail)
            else subWithDifferentTypesAllowed(op)(evaluated, tail)
          }

      private def subWithSameTypeOnly(op: String): (Expr, List[Expr]) => F[Expr] =
        (head, tail) =>
          head match
            case Expr.RInt(a)    => tail.foldExtract(a)(_ - _).bimap(_.opError(op, head :: tail), _.toExpr).liftTo[F]
            case Expr.RLong(a)   => tail.foldExtract(a)(_ - _).bimap(_.opError(op, head :: tail), _.toExpr).liftTo[F]
            case Expr.RFloat(a)  => tail.foldExtract(a)(_ - _).bimap(_.opError(op, head :: tail), _.toExpr).liftTo[F]
            case Expr.RDouble(a) => tail.foldExtract(a)(_ - _).bimap(_.opError(op, head :: tail), _.toExpr).liftTo[F]
            case other           => F.raiseError(FailureReason.InvalidArgumentType("Numeric", other).opError(op, head :: tail))

      private def subWithDifferentTypesAllowed(op: String): (Expr, List[Expr]) => F[Expr] =
        (head, tail) => tail.foldLeftM(head)((acc, expr) => subWithDifferentTypesAllowed(acc, expr)).leftMap(_.opError(op, head :: tail)).liftTo[F]

      private val subWithDifferentTypesAllowed: (Expr, Expr) => Either[FailureReason, Expr] =
        // same type
        case (Expr.RInt(a), Expr.RInt(b))       => (a - b).toExpr.asRight
        case (Expr.RLong(a), Expr.RLong(b))     => (a - b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RFloat(b))   => (a - b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RDouble(b)) => (a - b).toExpr.asRight
        // different types
        case (Expr.RInt(a), Expr.RLong(b))      => (a - b).toExpr.asRight
        case (Expr.RInt(a), Expr.RFloat(b))     => (a - b).toExpr.asRight
        case (Expr.RInt(a), Expr.RDouble(b))    => (a - b).toExpr.asRight
        case (Expr.RLong(a), Expr.RInt(b))      => (a - b).toExpr.asRight
        case (Expr.RLong(a), Expr.RFloat(b))    => (a - b).toExpr.asRight
        case (Expr.RLong(a), Expr.RDouble(b))   => (a - b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RInt(b))     => (a - b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RLong(b))    => (a - b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RDouble(b))  => (a - b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RInt(b))    => (a - b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RLong(b))   => (a - b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RFloat(b))  => (a - b).toExpr.asRight
        case (l, r)                             => FailureReason.InvalidArgumentTypes("Numeric", List(l, r)).asLeft
