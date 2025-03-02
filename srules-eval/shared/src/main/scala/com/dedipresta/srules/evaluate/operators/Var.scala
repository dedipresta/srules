package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Var:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError], ctxReader: UserContextReader[F, Ctx]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[F, Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): F[Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.with1Or2[F](op))
          .flatMap {
            case (Expr.RString(name), defaultValue) =>
              name match {
                // look for special variables (built-in or from the expression itself)
                case BuiltInVarName.ArrayIndex                       =>
                  ctx.indexValue.map(_.toExpr).toRight(FailureReason.VariableNotFound(name).opError(op, args)).liftTo[F]
                case BuiltInVarName.ArrayValue                       =>
                  ctx.currentValue.toRight(FailureReason.VariableNotFound(name).opError(op, args)).liftTo[F]
                case BuiltInVarName.Accumulator                      =>
                  ctx.accumulatorValue.toRight(FailureReason.VariableNotFound(name).opError(op, args)).liftTo[F]
                case named if named.startsWith(BuiltInVarName.Named) =>
                  val key = named.drop(BuiltInVarName.Named.length)
                  ctx.namedValue(key).toRight(FailureReason.VariableNotFound(name).opError(op, args)).liftTo[F]

                // look for variables in the user context, so it depends on the context type
                case other =>
                  ctxReader
                    .read(ctx.user, other, defaultValue)
                    // recover error to provide better context
                    .recoverWith { case EvaluationError.OperationFailure(op, args, e) => F.raiseError(e.opError(op, args)) }
              }
            case (other, _)                         => F.raiseError(FailureReason.InvalidArgumentType("String", other).opError(op, args))
          }
