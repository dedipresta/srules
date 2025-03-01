package com.dedipresta.srules.evaluate

import com.dedipresta.srules.Expr
import com.dedipresta.srules.evaluate.syntax.*

import cats.syntax.all.*

trait ExprEvaluator[Ctx, E]:
  // real implementation will own a map of operators (key=name, value=Operator)
  // when evaluating an expression, it will extract the operator name retrieve the operator from the map
  // then call operator.evaluate(this, op, args, ctx)

  // lazy version of evaluation, it evaluates only what is needed by operators and does not evaluate list sub-expressions
  final def evaluate(expr: Expr, ctx: Ctx): Either[E, Expr] =
    evaluate(expr, RuleCtx.DefaultContext(ctx, Map.empty))

  // full evaluation, it evaluates all sub-expressions of the expression
  final def evaluateAll(expr: Expr, ctx: Ctx): Either[E, Expr] =
    evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty))

  def evaluate(expr: Expr, ctx: RuleCtx[Ctx]): Either[E, Expr]

  def evaluateAll(expr: Expr, ctx: RuleCtx[Ctx]): Either[E, Expr]

final class ExprEvaluatorImpl[Ctx](
    operators: Map[String, Operator[Ctx, EvaluationError]],
) extends ExprEvaluator[Ctx, EvaluationError]:

  // full evaluation, it evaluates all sub-expressions of the expression
  def evaluateAll(expr: Expr, ctx: RuleCtx[Ctx]): Either[EvaluationError, Expr] =
    this.deepEvaluateFunctionsAndLists(expr, ctx)

  def evaluateAllAs[T: ExprExtractor](expr: Expr, ctx: Ctx): Either[EvaluationError, T] =
    evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty))
      .flatMap(ev => summon[ExprExtractor[T]].extract(ev).leftMap(_.opError("evaluateAs", List(ev))))

  def evaluateAllAsList[T: ExprExtractor](expr: Expr, ctx: Ctx): Either[EvaluationError, List[T]] =
    evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty)).flatMap {
      case Expr.RList(list) => list.traverse(evaluateAllAs[T](_, ctx))
      case other            => Left(EvaluationError.OperationFailure("evaluateAsList", List(other), FailureReason.InvalidArgumentType("List", other)))
    }

  // lazy version of evaluation, it evaluates only what is needed by operators and does not evaluate list sub-expressions
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
