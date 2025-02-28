package com.dedipresta.srules.evaluate.syntax

import com.dedipresta.srules.Expr
import com.dedipresta.srules.evaluate.*

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

  given ExprExtractor[Boolean]    = {
    case Expr.RBoolean(i) => Right(i)
    case expr             => Left(FailureReason.InvalidArgumentType("Boolean", expr))
  }
  given ExprExtractor[List[Expr]] = {
    case Expr.RList(i) => Right(i)
    case expr          => Left(FailureReason.InvalidArgumentType("List", expr))
  }

extension (args: List[Expr]) {

  def atLeast(n: Int, op: String): Either[EvaluationError, Unit] =
    if args.length < n then Left(EvaluationError.AtLeast(op, n, args)) else Right(())
  def atMost(n: Int, op: String): Either[EvaluationError, Unit]  =
    if args.length > n then Left(EvaluationError.AtMost(op, n, args)) else Right(())
  def exactly(n: Int, op: String): Either[EvaluationError, Unit] =
    if args.length != n then Left(EvaluationError.Exactly(op, n, args)) else Right(())

  def withExactly0(op: String): Either[EvaluationError, Unit]               =
    args.exactly(0, op)
  def withExactly1(op: String): Either[EvaluationError, Expr]               =
    args.exactly(1, op).as(args.head)
  def withExactly2(op: String): Either[EvaluationError, (Expr, Expr)]       =
    args.exactly(2, op).as(args.head, args(1))
  def withExactly3(op: String): Either[EvaluationError, (Expr, Expr, Expr)] =
    args.exactly(3, op).as(args.head, args(1), args(2))

  def withAtLeast1(op: String): Either[EvaluationError, (Expr, List[Expr])] =
    args.atLeast(1, op).as(args.head, args.tail)

  def withAtLeast2(op: String): Either[EvaluationError, (Expr, Expr, List[Expr])] =
    args.atLeast(2, op).as(args.head, args(1), args.drop(2))

  def withOptional(op: String): Either[EvaluationError, Option[Expr]] =
    args match {
      case Nil      => None.asRight
      case h :: Nil => Some(h).asRight
      case _        => EvaluationError.AtMost(op, 1, args).asLeft
    }

  def with1Or2(op: String): Either[EvaluationError, (Expr, Option[Expr])]       =
    args match {
      case h :: Nil      => (h, None).asRight
      case h :: t :: Nil => (h, Some(t)).asRight
      case _             => EvaluationError.AtLeast(op, 1, args).asLeft
    }
  def with2Or3(op: String): Either[EvaluationError, (Expr, Expr, Option[Expr])] =
    args match {
      case h :: t :: Nil      => (h, t, None).asRight
      case h :: t :: o :: Nil => (h, t, Some(o)).asRight
      case _                  => EvaluationError.AtLeast(op, 2, args).asLeft
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

extension (args: List[Expr]) {

  def values[T: ExprExtractor]: Either[FailureReason, List[T]] =
    args.traverse(summon[ExprExtractor[T]].extract(_))

  def ints: Either[FailureReason, List[Int]]         = args.values[Int]
  def longs: Either[FailureReason, List[Long]]       = args.values[Long]
  def floats: Either[FailureReason, List[Float]]     = args.values[Float]
  def doubles: Either[FailureReason, List[Double]]   = args.values[Double]
  def strings: Either[FailureReason, List[String]]   = args.values[String]
  def booleans: Either[FailureReason, List[Boolean]] = args.values[Boolean]

  def foldWhile[T, E](start: T)(f: (T, Expr) => Either[E, (T, Boolean)]): Either[E, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk) (acc, stillOk).asRight else f(acc, expr)
    }

  def foldExtractWhile[T: ExprExtractor](start: T)(f: (T, T) => (T, Boolean)): Either[FailureReason, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk) (acc, stillOk).asRight else summon[ExprExtractor[T]].extract(expr).map(f(acc, _))
    }

  def foldExtract[T: ExprExtractor](start: T)(f: (T, T) => T): Either[FailureReason, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        extracted <- summon[ExprExtractor[T]].extract(expr)
      } yield f(acc, extracted),
    )

  def foldExtractEither[T: ExprExtractor](start: T)(f: (T, T) => Either[FailureReason, T]): Either[FailureReason, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        extracted <- summon[ExprExtractor[T]].extract(expr)
        newValue  <- f(acc, extracted)
      } yield newValue,
    )

  def foldEvaluateExtract[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      op: String,
      ctx: RuleCtx[Ctx],
  )(start: T)(f: (T, T) => T): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        evaluated <- evaluator.evaluate(expr, ctx)
        extracted <- summon[ExprExtractor[T]].extract(evaluated).leftMap(EvaluationError.OperationFailure(op, args, _))
      } yield f(acc, extracted),
    )

  def foldEvaluateExtractEither[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      op: String,
      ctx: RuleCtx[Ctx],
  )(start: T)(f: (T, T) => Either[EvaluationError, T]): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        evaluated <- evaluator.evaluate(expr, ctx)
        extracted <- summon[ExprExtractor[T]].extract(evaluated).leftMap(EvaluationError.OperationFailure(op, args, _))
        newValue  <- f(acc, extracted)
      } yield newValue,
    )

  def foldEvaluateExtractWhile[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      op: String,
      ctx: RuleCtx[Ctx],
  )(start: T)(f: (T, T) => (T, Boolean)): Either[EvaluationError, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk)
        (acc, stillOk).asRight
      else
        for {
          evaluated <- evaluator.evaluate(expr, ctx)
          extracted <- summon[ExprExtractor[T]].extract(evaluated).leftMap(EvaluationError.OperationFailure(op, args, _))
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
