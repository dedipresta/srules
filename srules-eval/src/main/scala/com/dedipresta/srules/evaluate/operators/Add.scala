package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Add:

  def apply[F[_], Ctx](
      sameTypeOnly: Boolean = false,
      noArgsToZero: Boolean = true,
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
          .flatMap {
            case Nil   => if noArgsToZero then F.pure(Expr.RInt(0)) else F.raiseError(FailureReason.InvalidArgumentsCount.atLeast(1, 0).opError(op, Nil))
            case other =>
              other
                .withAtLeast1[F](op)
                .flatMap(addFn(_, _).leftMap(_.opError(op, args)).liftTo[F])
          }

      private val addWithSameTypeOnly: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        case (Expr.RInt(a), tail)    => tail.foldExtract[Int](a)(_ + _).map(_.toExpr)
        case (Expr.RLong(a), tail)   => tail.foldExtract[Long](a)(_ + _).map(_.toExpr)
        case (Expr.RFloat(a), tail)  => tail.foldExtract[Float](a)(_ + _).map(_.toExpr)
        case (Expr.RDouble(a), tail) => tail.foldExtract[Double](a)(_ + _).map(_.toExpr)
        case (Expr.RString(a), tail) => tail.foldExtract[String](a)(_ + _).map(_.toExpr)
        case (other, _)              => Left(FailureReason.InvalidArgumentType("Numeric", other))

      private val addWithDifferentTypesAllowedFold: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        (head, tail) => tail.foldLeftM(head)((acc, expr) => addWithDifferentTypesAllowed(acc, expr))

      private val addWithDifferentTypesAllowed: (Expr, Expr) => Either[FailureReason, Expr] =
        // same type
        case (Expr.RInt(a), Expr.RInt(b))       => (a + b).toExpr.asRight
        case (Expr.RLong(a), Expr.RLong(b))     => (a + b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RFloat(b))   => (a + b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RDouble(b)) => (a + b).toExpr.asRight
        case (Expr.RString(a), Expr.RString(b)) => (a + b).toExpr.asRight
        // different types
        case (Expr.RInt(a), Expr.RLong(b))      => (a + b).toExpr.asRight
        case (Expr.RInt(a), Expr.RFloat(b))     => (a + b).toExpr.asRight
        case (Expr.RInt(a), Expr.RDouble(b))    => (a + b).toExpr.asRight
        case (Expr.RLong(a), Expr.RInt(b))      => (a + b).toExpr.asRight
        case (Expr.RLong(a), Expr.RFloat(b))    => (a + b).toExpr.asRight
        case (Expr.RLong(a), Expr.RDouble(b))   => (a + b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RInt(b))     => (a + b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RLong(b))    => (a + b).toExpr.asRight
        case (Expr.RFloat(a), Expr.RDouble(b))  => (a + b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RInt(b))    => (a + b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RLong(b))   => (a + b).toExpr.asRight
        case (Expr.RDouble(a), Expr.RFloat(b))  => (a + b).toExpr.asRight
        case (l, r)                             => FailureReason.InvalidArgumentTypes("Numeric or String", List(l, r)).asLeft

      private val addFn: (Expr, List[Expr]) => Either[FailureReason, Expr] =
        if (sameTypeOnly) addWithSameTypeOnly else addWithDifferentTypesAllowedFold
