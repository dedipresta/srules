package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.*
import cats.syntax.all.*

import scala.concurrent.*
import scala.util.*

final class ExprEvaluatorImpl[F[_], Ctx](
    operators: Map[String, Operator[F, Ctx, EvaluationError]],
)(using F: MonadError[F, EvaluationError])
    extends ExprEvaluator[F, Ctx, EvaluationError]:

  // full evaluation, it evaluates all sub-expressions of the expression
  def evaluateAll(expr: Expr, ctx: RuleCtx[Ctx]): F[Expr] =
    this.deepEvaluateFunctionsAndLists(expr, ctx)

  def evaluateAllAs[T: ExprExtractor](expr: Expr, ctx: Ctx): F[T] =
    for {
      ev <- evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty))
      t  <- summon[ExprExtractor[T]].extract(ev).leftMap(_.opError("evaluateAs", List(ev))).liftTo[F]
    } yield t

  def evaluateAllAsList[T: ExprExtractor](expr: Expr, ctx: Ctx): F[List[T]] =
    evaluateAll(expr, RuleCtx.DefaultContext(ctx, Map.empty)).flatMap {
      case Expr.RList(list) => list.traverse(evaluateAllAs[T](_, ctx))
      case other            => F.raiseError(EvaluationError.OperationFailure("evaluateAsList", List(other), FailureReason.InvalidArgumentType("List", other)))
    }

  // lazy version of evaluation, it evaluates only what is needed by operators and does not evaluate list sub-expressions
  def evaluate(expr: Expr, ctx: RuleCtx[Ctx]): F[Expr] =
    expr match
      case Expr.RFunction(op, args) =>              // operator found, so evaluate
        operators.get(op) match
          case Some(operator) => operator.evaluate(this, op, args, ctx)
          case None           => F.raiseError(EvaluationError.OperatorNotFound(op, expr))
      case _                        => expr.pure[F] // already a value

object ExprEvaluatorImpl:

  given (using ec: ExecutionContext): MonadError[Future, EvaluationError] =
    new MonadError[Future, EvaluationError] {
      def pure[A](a: A): Future[A]                                      = Future.successful(a)
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B]    = fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] =
        f(a).flatMap {
          case Left(nextA) => tailRecM(nextA)(f)
          case Right(b)    => Future.successful(b)
        }

      def raiseError[A](e: EvaluationError): Future[A]                                  =
        Future.failed(e.toException)
      def handleErrorWith[A](fa: Future[A])(f: EvaluationError => Future[A]): Future[A] =
        fa.recoverWith { case EvaluationError.EvaluationException(err) => f(err) }
    }

  given MonadError[Try, EvaluationError] =
    new MonadError[Try, EvaluationError] {
      def pure[A](a: A): Try[A]                                                = Success(a)
      def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B]                    = fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B]              =
        f(a).flatMap {
          case Left(nextA) => tailRecM(nextA)(f)
          case Right(b)    => Success(b)
        }
      def raiseError[A](e: EvaluationError): Try[A]                            = Failure(e.toException)
      def handleErrorWith[A](fa: Try[A])(f: EvaluationError => Try[A]): Try[A] =
        fa.recoverWith { case EvaluationError.EvaluationException(err) => f(err) }
    }
