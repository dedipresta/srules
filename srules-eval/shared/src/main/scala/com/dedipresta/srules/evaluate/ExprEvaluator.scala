package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr

trait ExprEvaluator[Ctx, E]:
  // real implementation will own a map of operators (key=name, value=Operator)
  // when evaluating an expression, it will extract the operator name retrieve the operator from the map
  // then call operator.evaluate(this, op, args, ctx)
  final def evaluate(expr: Expr, ctx: Ctx): Either[E, Expr] =
    evaluate(expr, RuleCtx.DefaultContext(ctx, Map.empty))

  def evaluate(expr: Expr, ctx: RuleCtx[Ctx]): Either[E, Expr]

final class ExprEvaluatorImpl[Ctx](
    operators: Map[String, Operator[Ctx, EvaluationError]],
) extends ExprEvaluator[Ctx, EvaluationError]:

  def evaluate(expr: Expr, ctx: RuleCtx[Ctx]): Either[EvaluationError, Expr] =
    expr match
      case Expr.RFunction(op, args) =>             // operator found, so evaluate
        operators.get(op) match
          case Some(operator) => operator.evaluate(this, op, args, ctx)
          case None           => Left(EvaluationError.OperatorNotFound(op))
      case _                        => Right(expr) // already a value

object ExprEvaluatorImpl:
  def withOperators[Ctx](
      operators: Map[String, Operator[Ctx, EvaluationError]],
  ): ExprEvaluatorImpl[Ctx] = new ExprEvaluatorImpl(operators)
