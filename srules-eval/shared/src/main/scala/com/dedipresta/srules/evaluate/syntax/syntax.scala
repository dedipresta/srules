package com.dedipresta.srules.evaluate.syntax

import com.dedipresta.srules.Expr
import com.dedipresta.srules.evaluate.*

import cats.*
import cats.syntax.all.*

trait ExprExtractor[T]:
  def extract(op: String, expr: Expr): Either[EvaluationError, T]
object ExprExtractor:
  given ExprExtractor[Int] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RInt(i) => Right(i)
        case _            => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[Long] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RLong(i) => Right(i)
        case _             => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[Double] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RDouble(i) => Right(i)
        case _               => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[Float] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RFloat(i) => Right(i)
        case _              => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[String] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RString(i) => Right(i)
        case _               => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[(String, List[Expr])] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RFunction(name, args) => Right((name, args))
        case _                          => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

  given ExprExtractor[Boolean] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RBoolean(i) => Right(i)
        case _                => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }
  given ExprExtractor[List[Expr]] =
    (op: String, expr: Expr) =>
      expr match {
        case Expr.RList(i) => Right(i)
        case _             => Left(EvaluationError.InvalidArgumentType(op, List(expr)))
      }

extension (args: List[Expr]) {

  def atLeast(n: Int, op: String): Either[EvaluationError, Unit] =
    if args.length < n then Left(EvaluationError.AtLeast(op, n, args)) else Right(())
  def atMost(n: Int, op: String): Either[EvaluationError, Unit]  =
    if args.length > n then Left(EvaluationError.AtMost(op, n, args)) else Right(())
  def exactly(n: Int, op: String): Either[EvaluationError, Unit] =
    if args.length != n then Left(EvaluationError.Exactly(op, n, args)) else Right(())
    
  def withExactly1(op: String): Either[EvaluationError, Expr] =
    args.exactly(1, op).as(args.head)
    
  def withExactly2(op: String): Either[EvaluationError, (Expr, Expr)] =
    args.exactly(2, op).as(args.head, args(1))

}

extension (expr: Expr) {

  def mapInt[T](op: String, f: Int => T): Either[EvaluationError, T]                       =
    summon[ExprExtractor[Int]].extract(op, expr).map(f)
  def mapLong[T](op: String, f: Long => T): Either[EvaluationError, T]                     =
    summon[ExprExtractor[Long]].extract(op, expr).map(f)
  def mapFloat[T](op: String, f: Float => T): Either[EvaluationError, T]                   =
    summon[ExprExtractor[Float]].extract(op, expr).map(f)
  def mapDouble[T](op: String, f: Double => T): Either[EvaluationError, T]                 =
    summon[ExprExtractor[Double]].extract(op, expr).map(f)
  def mapString[T](op: String, f: String => T): Either[EvaluationError, T]                 =
    summon[ExprExtractor[String]].extract(op, expr).map(f)
  def mapBoolean[T](op: String, f: Boolean => T): Either[EvaluationError, T]               =
    summon[ExprExtractor[Boolean]].extract(op, expr).map(f)
  def mapFunction[T](op: String, f: (String, List[Expr]) => T): Either[EvaluationError, T] =
    summon[ExprExtractor[(String, List[Expr])]].extract(op, expr).map(f.tupled)
  def mapList[T](op: String, f: List[Expr] => T): Either[EvaluationError, T]               =
    summon[ExprExtractor[List[Expr]]].extract(op, expr).map(f)

  def flatMapInt[T](op: String, f: Int => Either[EvaluationError, T]): Either[EvaluationError, T]                       =
    summon[ExprExtractor[Int]].extract(op, expr).flatMap(f)
  def flatMapLong[T](op: String, f: Long => Either[EvaluationError, T]): Either[EvaluationError, T]                     =
    summon[ExprExtractor[Long]].extract(op, expr).flatMap(f)
  def flatMapFloat[T](op: String, f: Float => Either[EvaluationError, T]): Either[EvaluationError, T]                   =
    summon[ExprExtractor[Float]].extract(op, expr).flatMap(f)
  def flatMapDouble[T](op: String, f: Double => Either[EvaluationError, T]): Either[EvaluationError, T]                 =
    summon[ExprExtractor[Double]].extract(op, expr).flatMap(f)
  def flatMapString[T](op: String, f: String => Either[EvaluationError, T]): Either[EvaluationError, T]                 =
    summon[ExprExtractor[String]].extract(op, expr).flatMap(f)
  def flatMapBoolean[T](op: String, f: Boolean => Either[EvaluationError, T]): Either[EvaluationError, T]               =
    summon[ExprExtractor[Boolean]].extract(op, expr).flatMap(f)
  def flatMapFunction[T](op: String, f: (String, List[Expr]) => Either[EvaluationError, T]): Either[EvaluationError, T] =
    summon[ExprExtractor[(String, List[Expr])]].extract(op, expr).flatMap(f.tupled)
  def flatMapList[T](op: String, f: List[Expr] => Either[EvaluationError, T]): Either[EvaluationError, T]               =
    summon[ExprExtractor[List[Expr]]].extract(op, expr).flatMap(f)
}

extension (args: List[Expr]) {

  def values[T: ExprExtractor](op: String): Either[EvaluationError, List[T]] =
    args.traverse(summon[ExprExtractor[T]].extract(op, _))

  def ints(op: String): Either[EvaluationError, List[Int]]         = args.values[Int](op)
  def longs(op: String): Either[EvaluationError, List[Long]]       = args.values[Long](op)
  def floats(op: String): Either[EvaluationError, List[Float]]     = args.values[Float](op)
  def doubles(op: String): Either[EvaluationError, List[Double]]   = args.values[Double](op)
  def strings(op: String): Either[EvaluationError, List[String]]   = args.values[String](op)
  def booleans(op: String): Either[EvaluationError, List[Boolean]] = args.values[Boolean](op)

  def foldWhile[T, E](start: T)(f: (T, Expr) => Either[E, (T, Boolean)]): Either[E, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk) (acc, stillOk).asRight else f(acc, expr)
    }

  def foldExtractWhile[T: ExprExtractor](op: String, start: T)(f: (T, T) => (T, Boolean)): Either[EvaluationError, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk) (acc, stillOk).asRight else summon[ExprExtractor[T]].extract(op, expr).map(f(acc, _))
    }

  def foldExtract[T: ExprExtractor](op: String, start: T)(f: (T, T) => T): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        extracted <- summon[ExprExtractor[T]].extract(op, expr)
      } yield f(acc, extracted),
    )

  def foldExtractEither[T: ExprExtractor](op: String, start: T)(f: (T, T) => Either[EvaluationError, T]): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        extracted <- summon[ExprExtractor[T]].extract(op, expr)
        newValue  <- f(acc, extracted)
      } yield newValue,
    )

  def foldEvaluateExtract[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      ctx: RuleCtx[Ctx],
  )(op: String, start: T)(f: (T, T) => T): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        evaluated <- evaluator.evaluate(expr, ctx)
        extracted <- summon[ExprExtractor[T]].extract(op, evaluated)
      } yield f(acc, extracted),
    )

  def foldEvaluateExtractEither[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      ctx: RuleCtx[Ctx],
  )(op: String, start: T)(f: (T, T) => Either[EvaluationError, T]): Either[EvaluationError, T] =
    args.foldLeftM(start)((acc, expr) =>
      for {
        evaluated <- evaluator.evaluate(expr, ctx)
        extracted <- summon[ExprExtractor[T]].extract(op, evaluated)
        newValue  <- f(acc, extracted)
      } yield newValue,
    )

  def foldEvaluateExtractWhile[T: ExprExtractor, Ctx](
      evaluator: ExprEvaluator[Ctx, EvaluationError],
      ctx: RuleCtx[Ctx],
  )(op: String, start: T)(f: (T, T) => (T, Boolean)): Either[EvaluationError, (T, Boolean)] =
    args.foldLeftM((start, true)) { case ((acc, stillOk), expr) =>
      if (!stillOk)
        (acc, stillOk).asRight
      else
        for {
          evaluated <- evaluator.evaluate(expr, ctx)
          extracted <- summon[ExprExtractor[T]].extract(op, evaluated)
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
  inline def toExpr: Expr.RString = Expr.RString(s)
}
extension (b: Boolean) {
  inline def toExpr: Expr.RBoolean = Expr.RBoolean(b)
}
extension (l: List[Expr]) {
  inline def toExpr: Expr.RList = Expr.RList(l)
}
