package com.dedipresta.srules.evaluate.syntax

import com.dedipresta.srules.Expr
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.FailureReason.InvalidArgumentsCount

import cats.*
import cats.syntax.all.*

trait ExprExtractor[T]:
  def extract(expr: Expr): Either[FailureReason, T]
object ExprExtractor:

  given ExprExtractor[Int] = {
    case Expr.RInt(i) => Right(i)
    case expr         => Left(FailureReason.InvalidArgumentType("Int", expr))
  }

  given ExprExtractor[Long] = {
    case Expr.RLong(i) => Right(i)
    case expr          => Left(FailureReason.InvalidArgumentType("Long", expr))
  }

  given ExprExtractor[Double] = {
    case Expr.RDouble(i) => Right(i)
    case expr            => Left(FailureReason.InvalidArgumentType("Double", expr))
  }

  given ExprExtractor[Float] = {
    case Expr.RFloat(i) => Right(i)
    case expr           => Left(FailureReason.InvalidArgumentType("Float", expr))
  }

  given ExprExtractor[String] = {
    case Expr.RString(i) => Right(i)
    case expr            => Left(FailureReason.InvalidArgumentType("String", expr))
  }

  given ExprExtractor[(String, List[Expr])] = {
    case Expr.RFunction(name, args) => Right((name, args))
    case expr                       => Left(FailureReason.InvalidArgumentType("Function", expr))
  }

  given ExprExtractor[Boolean] = {
    case Expr.RBoolean(i) => Right(i)
    case expr             => Left(FailureReason.InvalidArgumentType("Boolean", expr))
  }

  given ExprExtractor[List[Expr]] = {
    case Expr.RList(i) => Right(i)
    case expr          => Left(FailureReason.InvalidArgumentType("List", expr))
  }

extension (args: List[Expr]) {

  def atLeast[F[_]](n: Int, op: String)(using F: MonadError[F, EvaluationError]): F[Unit] =
    if args.length < n then F.raiseError(InvalidArgumentsCount.atLeast(n, args.length).opError(op, args)) else ().pure[F]
  def atMost[F[_]](n: Int, op: String)(using F: MonadError[F, EvaluationError]): F[Unit]  =
    if args.length > n then F.raiseError(InvalidArgumentsCount.atMost(n, args.length).opError(op, args)) else ().pure[F]
  def exactly[F[_]](n: Int, op: String)(using F: MonadError[F, EvaluationError]): F[Unit] =
    if args.length != n then F.raiseError(InvalidArgumentsCount.exactly(n, args.length).opError(op, args)) else ().pure[F]

  def withExactly0[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[Unit]               =
    args.exactly(0, op)
  def withExactly1[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[Expr]               =
    args.exactly(1, op).map(_ => args.head)
  def withExactly2[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, Expr)]       =
    args.exactly(2, op).map(_ => (args.head, args(1)))
  def withExactly3[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, Expr, Expr)] =
    args.exactly(3, op).map(_ => (args.head, args(1), args(2)))

  def withAtLeast1[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, List[Expr])] =
    args.atLeast(1, op).map(_ => (args.head, args.drop(1)))

  def withAtLeast2[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, Expr, List[Expr])] =
    args.atLeast(2, op).map(_ => (args.head, args(1), args.drop(2)))

  def withOptional[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[Option[Expr]] =
    args match {
      case Nil      => None.pure[F]
      case h :: Nil => Some(h).pure[F]
      case _        => F.raiseError(InvalidArgumentsCount.atMost(1, args.length).opError(op, args))
    }

  def with1Or2[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, Option[Expr])]       =
    args match {
      case h :: Nil      => (h, None).pure[F]
      case h :: t :: Nil => (h, Some(t)).pure[F]
      case _             => F.raiseError(InvalidArgumentsCount.atMost(2, args.length).opError(op, args))
    }
  def with2Or3[F[_]](op: String)(using F: MonadError[F, EvaluationError]): F[(Expr, Expr, Option[Expr])] =
    args match {
      case h :: t :: Nil      => (h, t, None).pure[F]
      case h :: t :: o :: Nil => (h, t, Some(o)).pure[F]
      case _                  => F.raiseError(InvalidArgumentsCount.atMost(3, args.length).opError(op, args))
    }

}

extension (expr: Expr) {

  def withBoolean: Either[FailureReason, Boolean]               = summon[ExprExtractor[Boolean]].extract(expr)
  def withInt: Either[FailureReason, Int]                       = summon[ExprExtractor[Int]].extract(expr)
  def withLong: Either[FailureReason, Long]                     = summon[ExprExtractor[Long]].extract(expr)
  def withFloat: Either[FailureReason, Float]                   = summon[ExprExtractor[Float]].extract(expr)
  def withDouble: Either[FailureReason, Double]                 = summon[ExprExtractor[Double]].extract(expr)
  def withString: Either[FailureReason, String]                 = summon[ExprExtractor[String]].extract(expr)
  def withFunction: Either[FailureReason, (String, List[Expr])] = summon[ExprExtractor[(String, List[Expr])]].extract(expr)
  def withList: Either[FailureReason, List[Expr]]               = summon[ExprExtractor[List[Expr]]].extract(expr)
  def withNumericToDouble: Either[FailureReason, Double]        =
    expr match {
      case Expr.RInt(i)    => Right(i.toDouble)
      case Expr.RLong(i)   => Right(i.toDouble)
      case Expr.RFloat(i)  => Right(i.toDouble)
      case Expr.RDouble(i) => Right(i)
      case _               => Left(FailureReason.InvalidArgumentType("Numeric", expr))
    }

  def mapInt[T](f: Int => T): Either[FailureReason, T]                       = withInt.map(f)
  def mapLong[T](f: Long => T): Either[FailureReason, T]                     = withLong.map(f)
  def mapFloat[T](f: Float => T): Either[FailureReason, T]                   = withFloat.map(f)
  def mapDouble[T](f: Double => T): Either[FailureReason, T]                 = withDouble.map(f)
  def mapString[T](f: String => T): Either[FailureReason, T]                 = withString.map(f)
  def mapBoolean[T](f: Boolean => T): Either[FailureReason, T]               = withBoolean.map(f)
  def mapFunction[T](f: (String, List[Expr]) => T): Either[FailureReason, T] = withFunction.map(f.tupled)
  def mapList[T](f: List[Expr] => T): Either[FailureReason, T]               = withList.map(f)
}

extension [F[_], Ctx](ev: ExprEvaluator[F, Ctx, EvaluationError]) {

  def evaluatedToBoolean(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Boolean] =
    expr match {
      case v: Expr.RBoolean  => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToBoolean(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Boolean", expr)))
    }

  def evaluatedToInt(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Int] =
    expr match {
      case v: Expr.RInt      => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToInt(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Int", expr)))
    }

  def evaluatedToLong(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Long] =
    expr match {
      case v: Expr.RLong     => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToLong(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Long", expr)))
    }

  def evaluatedToFloat(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Float] =
    expr match {
      case v: Expr.RFloat    => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToFloat(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Float", expr)))
    }

  def evaluatedToDouble(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Double] =
    expr match {
      case v: Expr.RDouble   => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToDouble(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Double", expr)))
    }

  def evaluatedToString(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[String] =
    expr match {
      case v: Expr.RString   => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToString(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("String", expr)))
    }

  def evaluatedToList(op: String, expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[List[Expr]] =
    expr match {
      case v: Expr.RList     => v.value.pure[F]
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(evaluatedToList(op, _, ctx))
      case _                 => F.raiseError(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("List", expr)))
    }

  def deepEvaluateFunctions(expr: Expr, ctx: RuleCtx[Ctx])(using F: MonadError[F, EvaluationError]): F[Expr] =
    expr match {
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(deepEvaluateFunctions(_, ctx))
      case _                 => expr.pure[F]
    }

  def deepEvaluateFunctionsAndLists(expr: Expr, ctx: RuleCtx[Ctx])(using MonadError[F, EvaluationError]): F[Expr] =
    expr match {
      case f: Expr.RFunction => ev.evaluate(f, ctx).flatMap(deepEvaluateFunctionsAndLists(_, ctx))
      case l: Expr.RList     => l.value.traverse(deepEvaluateFunctionsAndLists(_, ctx)).map(_.toExpr)
      case _                 => expr.pure[F]
    }
}

extension (args: List[Expr]) {

  def values[T: ExprExtractor]: Either[FailureReason, List[T]] =
    args.traverse(summon[ExprExtractor[T]].extract(_))

  def ints: Either[FailureReason, List[Int]]         = args.values[Int]
  def longs: Either[FailureReason, List[Long]]       = args.values[Long]
  def floats: Either[FailureReason, List[Float]]     = args.values[Float]
  def doubles: Either[FailureReason, List[Double]]   = args.values[Double]
  def strings: Either[FailureReason, List[String]]   = args.values[String]
  def booleans: Either[FailureReason, List[Boolean]] = args.values[Boolean]
  def lists: Either[FailureReason, List[List[Expr]]] = args.values[List[Expr]]

  def foldExtract[T: ExprExtractor](start: T)(f: (T, T) => T): Either[FailureReason, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        extracted <- summon[ExprExtractor[T]].extract(expr)
      } yield f(acc, extracted),
    )

  def foldDeepEvaluateWhile[F[_], T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[F, Ctx, EvaluationError],
      op: String,
      ctx: RuleCtx[Ctx],
  )(start: T)(f: (T, T) => (T, Boolean))(using F: MonadError[F, EvaluationError]): F[(T, Boolean)] =
    args.foldLeftM[F, (T, Boolean)]((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk)
        (acc, stillOk).pure[F]
      else
        for {
          evaluated <- evaluator.deepEvaluateFunctions(expr, ctx)
          extracted <- summon[ExprExtractor[T]].extract(evaluated).leftMap(EvaluationError.OperationFailure(op, args, _)).liftTo[F]
        } yield f(acc, extracted)
    }

}

extension (i: Int) {
  inline def toExpr: Expr.RInt = Expr.RInt(i)
}
extension (l: Long) {
  inline def toExpr: Expr.RLong = Expr.RLong(l)
}
extension (f: Float) {
  inline def toExpr: Expr.RFloat = Expr.RFloat(f)
}
extension (d: Double) {
  inline def toExpr: Expr.RDouble = Expr.RDouble(d)
}
extension (s: String) {
  inline def toExpr: Expr.RString       = Expr.RString(s)
  inline def toVar: Expr.RFunction      = Expr.RFunction("var", List(Expr.RString(s)))
  inline def toNamedVar: Expr.RFunction = s"${BuiltInVarName.Named}$s".toVar
}
extension (b: Boolean) {
  inline def toExpr: Expr.RBoolean = Expr.RBoolean(b)
}
extension (l: List[Expr]) {
  inline def toExpr: Expr.RList = Expr.RList(l)
}

extension (v: Unit) {
  def toIndexVar: Expr.RFunction       = BuiltInVarName.ArrayIndex.toVar
  def toValueVar: Expr.RFunction       = BuiltInVarName.ArrayValue.toVar
  def toAccumulatorVar: Expr.RFunction = BuiltInVarName.Accumulator.toVar
}

extension [T <: Expr](e: Either[FailureReason, T]) {
  def opError(op: String, args: List[Expr]): Either[EvaluationError, T] =
    e.leftMap(reason => EvaluationError.OperationFailure(op, args, reason))
}

extension (e: FailureReason) {
  def opError(op: String, args: List[Expr]): EvaluationError =
    EvaluationError.OperationFailure(op, args, e)
}
