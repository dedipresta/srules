package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

object Var:

  def apply[Ctx: UserContextReader]: Operator[Ctx, EvaluationError] =
    new Operator[Ctx, EvaluationError]:
      def evaluate(
          evaluator: ExprEvaluator[Ctx, EvaluationError],
          op: String,
          args: List[Expr],
          ctx: RuleCtx[Ctx],
      ): Either[EvaluationError, Expr] =
        args
          .traverse(evaluator.deepEvaluateFunctions(_, ctx))
          .flatMap(_.with1Or2(op))
          .flatMap {
            case (Expr.RString(name), defaultValue) =>
              name match {
                // look for special variables (built-in or from the expression itself)
                case BuiltInVarName.ArrayIndex                       => ctx.indexValue.map(_.toExpr).toRight(FailureReason.VariableNotFound(name)).opError(op, args)
                case BuiltInVarName.ArrayValue                       => ctx.currentValue.toRight(FailureReason.VariableNotFound(name)).opError(op, args)
                case BuiltInVarName.Accumulator                      => ctx.accumulatorValue.toRight(FailureReason.VariableNotFound(name)).opError(op, args)
                case named if named.startsWith(BuiltInVarName.Named) =>
                  val key = named.drop(BuiltInVarName.Named.length)
                  ctx.namedValue(key).toRight(FailureReason.VariableNotFound(name)).opError(op, args)

                // look for variables in the user context, so it depends on the context type
                case other => summon[UserContextReader[Ctx]].read(ctx.user, other, defaultValue).opError(op, args)
              }
            case (other, _)                         => Left(FailureReason.InvalidArgumentType("String", other)).opError(op, args)
          }
