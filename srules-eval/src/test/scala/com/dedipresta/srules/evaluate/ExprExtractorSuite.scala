package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.FailureReason.InvalidArgumentsCount
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*
import com.dedipresta.srules.evaluate.syntax.ExprExtractor.given

import munit.*

final class ExprExtractorSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  test("ExprExtractor[Int] allows to extract an Int from an Expr (success)") {
    val expr: Expr = 42.toExpr
    assertEquals(summon[ExprExtractor[Int]].extract(expr), Right(42))
  }

  test("ExprExtractor[Int] allows to extract an Int from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(summon[ExprExtractor[Int]].extract(expr), Left(FailureReason.InvalidArgumentType("Int", expr)))
  }

  test("ExprExtractor[Boolean] allows to extract a Boolean from an Expr (success)") {
    val expr: Expr = true.toExpr
    assertEquals(summon[ExprExtractor[Boolean]].extract(expr), Right(true))
  }

  test("ExprExtractor[Boolean] allows to extract a Boolean from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(summon[ExprExtractor[Boolean]].extract(expr), Left(FailureReason.InvalidArgumentType("Boolean", expr)))
  }

  test("ExprExtractor[Long] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42L.toExpr
    assertEquals(summon[ExprExtractor[Long]].extract(expr), Right(42L))
  }

  test("ExprExtractor[Long] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(summon[ExprExtractor[Long]].extract(expr), Left(FailureReason.InvalidArgumentType("Long", expr)))
  }

  test("ExprExtractor[Float] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42.0f.toExpr
    assertEquals(summon[ExprExtractor[Float]].extract(expr), Right(42.0f))
  }

  test("ExprExtractor[Float] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(summon[ExprExtractor[Float]].extract(expr), Left(FailureReason.InvalidArgumentType("Float", expr)))
  }

  test("ExprExtractor[Double] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42.0.toExpr
    assertEquals(summon[ExprExtractor[Double]].extract(expr), Right(42.0))
  }

  test("ExprExtractor[Double] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(summon[ExprExtractor[Double]].extract(expr), Left(FailureReason.InvalidArgumentType("Double", expr)))
  }

  test("ExprExtractor[String] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = "hello".toExpr
    assertEquals(summon[ExprExtractor[String]].extract(expr), Right("hello"))
  }

  test("ExprExtractor[String] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(summon[ExprExtractor[String]].extract(expr), Left(FailureReason.InvalidArgumentType("String", expr)))
  }

  test("ExprExtractor[(String, List[Expr])] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = Expr.RFunction("foo", List(1.toExpr, 2.toExpr, 3.toExpr))
    assertEquals(summon[ExprExtractor[(String, List[Expr])]].extract(expr), Right(("foo", List(1.toExpr, 2.toExpr, 3.toExpr))))
  }

  test("ExprExtractor[(String, List[Expr])] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(summon[ExprExtractor[(String, List[Expr])]].extract(expr), Left(FailureReason.InvalidArgumentType("Function", expr)))
  }

  test("ExprExtractor[List[Expr]] allows to extract an Expr from an Expr (success)") {
    val expr: Expr = List(1.toExpr, 2.toExpr, 3.toExpr).toExpr
    assertEquals(summon[ExprExtractor[List[Expr]]].extract(expr), Right(List(1.toExpr, 2.toExpr, 3.toExpr)))
  }

  test("ExprExtractor[List[Expr]] allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(summon[ExprExtractor[List[Expr]]].extract(expr), Left(FailureReason.InvalidArgumentType("List", expr)))
  }

  val op = "test"

  test("atLeast on args list return Unit when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.atLeast[ErrorOr](3, op), Right(()))
  }

  test("atLeast on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.atLeast[ErrorOr](4, op), Left(InvalidArgumentsCount.atLeast(4, 3)).opError(op, args))
  }

  test("atMost on args list return Unit when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.atMost[ErrorOr](3, op), Right(()))
  }

  test("atMost on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.atMost[ErrorOr](2, op), Left(InvalidArgumentsCount.atMost(2, 3)).opError(op, args))
  }

  test("exactly on args list return Unit when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.exactly[ErrorOr](3, op), Right(()))
  }

  test("exactly on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.exactly[ErrorOr](2, op), Left(InvalidArgumentsCount.exactly(2, 3)).opError(op, args))
  }

  test("withExactly0 on args list return Unit when satisfied") {
    val args: List[Expr] = List.empty
    assertEquals(args.withExactly0[ErrorOr](op), Right(()))
  }

  test("withExactly0 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withExactly0[ErrorOr](op), Left(InvalidArgumentsCount.exactly(0, 3)).opError(op, args))
  }

  test("withExactly1 on args list return the argument when satisfied") {
    val args: List[Expr] = List(1.toExpr)
    assertEquals(args.withExactly1[ErrorOr](op), Right(1.toExpr))
  }

  test("withExactly1 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withExactly1[ErrorOr](op), Left(InvalidArgumentsCount.exactly(1, 3)).opError(op, args))
  }

  test("withExactly2 on args list return the arguments when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr)
    assertEquals(args.withExactly2[ErrorOr](op), Right((1.toExpr, 2.toExpr)))
  }

  test("withExactly2 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withExactly2[ErrorOr](op), Left(InvalidArgumentsCount.exactly(2, 3)).opError(op, args))
  }

  test("withExactly3 on args list return the arguments when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withExactly3[ErrorOr](op), Right((1.toExpr, 2.toExpr, 3.toExpr)))
  }

  test("withExactly3 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr)
    assertEquals(args.withExactly3[ErrorOr](op), Left(InvalidArgumentsCount.exactly(3, 2)).opError(op, args))
  }

  test("withAtLeast1 on args list return the arguments when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withAtLeast1[ErrorOr](op), Right((args.head, args.tail)))
  }

  test("withAtLeast1 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List.empty
    assertEquals(args.withAtLeast1[ErrorOr](op), Left(InvalidArgumentsCount.atLeast(1, 0)).opError(op, args))
  }

  test("withAtLeast2 on args list return the arguments when satisfied") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.withAtLeast2[ErrorOr](op), Right((args.head, args(1), args.drop(2))))
  }

  test("withAtLeast2 on args list return FailureReason.InvalidArgumentsCount when not satisfied") {
    val args: List[Expr] = List(1.toExpr)
    assertEquals(args.withAtLeast2[ErrorOr](op), Left(InvalidArgumentsCount.atLeast(2, 1)).opError(op, args))
  }

  test("withOptional on args list return the argument when present") {
    val args: List[Expr] = List(1.toExpr)
    assertEquals(args.withOptional[ErrorOr](op), Right(Some(1.toExpr)))
  }

  test("withOptional on args list return None when not present") {
    val args: List[Expr] = List.empty
    assertEquals(args.withOptional[ErrorOr](op), Right(None))
  }

  test("withOptional on args list return an error if more than one argument is present") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr)
    assertEquals(args.withOptional[ErrorOr](op), Left(InvalidArgumentsCount.atMost(1, 2)).opError(op, args))
  }

  test("with1Or2 on args list return the arguments when present") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr)
    assertEquals(args.with1Or2[ErrorOr](op), Right((args.head, args.drop(1).headOption)))
  }

  test("with1Or2 on args list return the argument when only one is present") {
    val args: List[Expr] = List(1.toExpr)
    assertEquals(args.with1Or2[ErrorOr](op), Right((args.head, None)))
  }

  test("with1Or2 on args list return FailureReason.InvalidArgumentsCount when none is present") {
    val args: List[Expr] = List.empty
    assertEquals(args.with1Or2[ErrorOr](op), Left(InvalidArgumentsCount.atMost(2, 0)).opError(op, args))
  }

  test("with2Or3 on args list return the arguments when present") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.with2Or3[ErrorOr](op), Right((args.head, args(1), args.drop(2).headOption)))
  }

  test("with2Or3 on args list return the arguments when only two are present") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr)
    assertEquals(args.with2Or3[ErrorOr](op), Right((args.head, args(1), None)))
  }

  test("with2Or3 on args list return FailureReason.InvalidArgumentsCount when none is present") {
    val args: List[Expr] = List.empty
    assertEquals(args.with2Or3[ErrorOr](op), Left(InvalidArgumentsCount.atMost(3, 0)).opError(op, args))
  }

  test("withBoolean allows to extract a Boolean from an Expr (success)") {
    val expr: Expr = true.toExpr
    assertEquals(expr.withBoolean, Right(true))
  }

  test("withBoolean allows to extract a Boolean from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withBoolean, Left(FailureReason.InvalidArgumentType("Boolean", expr)))
  }

  test("withInt allows to extract an Int from an Expr (success)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withInt, Right(42))
  }

  test("withInt allows to extract an Int from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withInt, Left(FailureReason.InvalidArgumentType("Int", expr)))
  }

  test("withLong allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42L.toExpr
    assertEquals(expr.withLong, Right(42L))
  }

  test("withLong allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withLong, Left(FailureReason.InvalidArgumentType("Long", expr)))
  }

  test("withFloat allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42.0f.toExpr
    assertEquals(expr.withFloat, Right(42.0f))
  }

  test("withFloat allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withFloat, Left(FailureReason.InvalidArgumentType("Float", expr)))
  }

  test("withDouble allows to extract an Expr from an Expr (success)") {
    val expr: Expr = 42.0.toExpr
    assertEquals(expr.withDouble, Right(42.0))
  }

  test("withDouble allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withDouble, Left(FailureReason.InvalidArgumentType("Double", expr)))
  }

  test("withString allows to extract an Expr from an Expr (success)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withString, Right("hello"))
  }

  test("withString allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withString, Left(FailureReason.InvalidArgumentType("String", expr)))
  }

  test("withFunction allows to extract an Expr from an Expr (success)") {
    val expr: Expr = Expr.RFunction("foo", List(1.toExpr, 2.toExpr, 3.toExpr))
    assertEquals(expr.withFunction, Right(("foo", List(1.toExpr, 2.toExpr, 3.toExpr))))
  }

  test("withFunction allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withFunction, Left(FailureReason.InvalidArgumentType("Function", expr)))
  }

  test("withList allows to extract an Expr from an Expr (success)") {
    val expr: Expr = List(1.toExpr, 2.toExpr, 3.toExpr).toExpr
    assertEquals(expr.withList, Right(List(1.toExpr, 2.toExpr, 3.toExpr)))
  }

  test("withList allows to extract an Expr from an Expr (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withList, Left(FailureReason.InvalidArgumentType("List", expr)))
  }

  test("withNumericToDouble allows to extract an Expr from an Expr (int success)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.withNumericToDouble, Right(42.0))
  }

  test("withNumericToDouble allows to extract an Expr from an Expr (long success)") {
    val expr: Expr = 42L.toExpr
    assertEquals(expr.withNumericToDouble, Right(42.0))
  }

  test("withNumericToDouble allows to extract an Expr from an Expr (float success)") {
    val expr: Expr = 42.0f.toExpr
    assertEquals(expr.withNumericToDouble, Right(42.0))
  }

  test("withNumericToDouble allows to extract an Expr from an Expr (double success)") {
    val expr: Expr = 42.0.toExpr
    assertEquals(expr.withNumericToDouble, Right(42.0))
  }

  test("withNumericToDouble allows to extract an Expr from an Expr (string failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.withNumericToDouble, Left(FailureReason.InvalidArgumentType("Numeric", expr)))
  }

  test("mapInt allow to perform a transformation on an Int (success)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.mapInt(_ + 1), Right(43))
  }

  test("mapInt allow to perform a transformation on an Int (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.mapInt(_ + 1), Left(FailureReason.InvalidArgumentType("Int", expr)))
  }

  test("mapLong allow to perform a transformation on a Long (success)") {
    val expr: Expr = 42L.toExpr
    assertEquals(expr.mapLong(_ + 1), Right(43L))
  }

  test("mapLong allow to perform a transformation on a Long (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.mapLong(_ + 1), Left(FailureReason.InvalidArgumentType("Long", expr)))
  }

  test("mapFloat allow to perform a transformation on a Float (success)") {
    val expr: Expr = 42.0f.toExpr
    assertEquals(expr.mapFloat(_ + 1), Right(43.0f))
  }

  test("mapFloat allow to perform a transformation on a Float (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.mapFloat(_ + 1), Left(FailureReason.InvalidArgumentType("Float", expr)))
  }

  test("mapDouble allow to perform a transformation on a Double (success)") {
    val expr: Expr = 42.0.toExpr
    assertEquals(expr.mapDouble(_ + 1), Right(43.0))
  }

  test("mapDouble allow to perform a transformation on a Double (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.mapDouble(_ + 1), Left(FailureReason.InvalidArgumentType("Double", expr)))
  }

  test("mapString allow to perform a transformation on a String (success)") {
    val expr: Expr = "hello".toExpr
    assertEquals(expr.mapString(_ + " world"), Right("hello world"))
  }

  test("mapString allow to perform a transformation on a String (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.mapString(_ + " world"), Left(FailureReason.InvalidArgumentType("String", expr)))
  }

  test("mapBoolean allow to perform a transformation on a Boolean (success)") {
    val expr: Expr = true.toExpr
    assertEquals(expr.mapBoolean(!_), Right(false))
  }

  test("mapBoolean allow to perform a transformation on a Boolean (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(expr.mapBoolean(!_), Left(FailureReason.InvalidArgumentType("Boolean", expr)))
  }

  test("mapFunction allow to perform a transformation on a Function (success)") {
    val expr: Expr                                      = Expr.RFunction("foo", List(1.toExpr, 2.toExpr, 3.toExpr))
    val f: (String, List[Expr]) => (String, List[Expr]) = (name, args) => (name + "!", args ::: List(4.toExpr))
    assertEquals(expr.mapFunction(f), Right(("foo!", List(1.toExpr, 2.toExpr, 3.toExpr, 4.toExpr))))
  }

  test("mapFunction allow to perform a transformation on a Function (failure)") {
    val expr: Expr                                      = 42.toExpr
    val f: (String, List[Expr]) => (String, List[Expr]) = (name, args) => (name + "!", args ::: List(4.toExpr))
    assertEquals(expr.mapFunction(f), Left(FailureReason.InvalidArgumentType("Function", expr)))
  }

  test("mapList allow to perform a transformation on a List (success)") {
    val expr: Expr                  = List(1.toExpr, 2.toExpr, 3.toExpr).toExpr
    val f: List[Expr] => List[Expr] = _ :+ 4.toExpr
    assertEquals(expr.mapList(f), Right(List(1.toExpr, 2.toExpr, 3.toExpr, 4.toExpr)))
  }

  test("mapList allow to perform a transformation on a List (failure)") {
    val expr: Expr                  = 42.toExpr
    val f: List[Expr] => List[Expr] = _ :+ 4.toExpr
    assertEquals(expr.mapList(f), Left(FailureReason.InvalidArgumentType("List", expr)))
  }

  test("evaluator.evaluatedToBoolean evaluate expr to Boolean deeply (success Boolean)") {
    val expr: Expr = true.toExpr
    assertEquals(evaluator.evaluatedToBoolean(op, expr, RuleCtx.empty(Map.empty)), Right(true))
  }

  test("evaluator.evaluatedToBoolean evaluate expr to Boolean deeply (success function that evaluates to Boolean)") {
    val expr: Expr = Expr.RFunction("==", List(1.toExpr, 1.toExpr))
    assertEquals(evaluator.evaluatedToBoolean(op, expr, RuleCtx.empty(Map.empty)), Right(true))
  }

  test("evaluator.evaluatedToBoolean evaluate expr to Boolean deeply (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(
      evaluator.evaluatedToBoolean(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Boolean", expr))),
    )
  }

  test("evaluator.evaluatedToInt evaluate expr to Int deeply (success Int)") {
    val expr: Expr = 42.toExpr
    assertEquals(evaluator.evaluatedToInt(op, expr, RuleCtx.empty(Map.empty)), Right(42))
  }

  test("evaluator.evaluatedToInt evaluate expr to Int deeply (success function that evaluates to Int)") {
    val expr: Expr = Expr.RFunction("+", List(1.toExpr, 1.toExpr))
    assertEquals(evaluator.evaluatedToInt(op, expr, RuleCtx.empty(Map.empty)), Right(2))
  }

  test("evaluator.evaluatedToInt evaluate expr to Int deeply (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(
      evaluator.evaluatedToInt(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Int", expr))),
    )
  }

  test("evaluator.evaluatedToLong evaluate expr to Long deeply (success Long)") {
    val expr: Expr = 42L.toExpr
    assertEquals(evaluator.evaluatedToLong(op, expr, RuleCtx.empty(Map.empty)), Right(42L))
  }

  test("evaluator.evaluatedToLong evaluate expr to Long deeply (success function that evaluates to Long)") {
    val expr: Expr = Expr.RFunction("+", List(1L.toExpr, 1L.toExpr))
    assertEquals(evaluator.evaluatedToLong(op, expr, RuleCtx.empty(Map.empty)), Right(2L))
  }

  test("evaluator.evaluatedToLong evaluate expr to Long deeply (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(
      evaluator.evaluatedToLong(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Long", expr))),
    )
  }

  test("evaluator.evaluatedToFloat evaluate expr to Float deeply (success Float)") {
    val expr: Expr = 42.0f.toExpr
    assertEquals(evaluator.evaluatedToFloat(op, expr, RuleCtx.empty(Map.empty)), Right(42.0f))
  }

  test("evaluator.evaluatedToFloat evaluate expr to Float deeply (success function that evaluates to Float)") {
    val expr: Expr = Expr.RFunction("+", List(1.0f.toExpr, 1.0f.toExpr))
    assertEquals(evaluator.evaluatedToFloat(op, expr, RuleCtx.empty(Map.empty)), Right(2.0f))
  }

  test("evaluator.evaluatedToFloat evaluate expr to Float deeply (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(
      evaluator.evaluatedToFloat(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Float", expr))),
    )
  }

  test("evaluator.evaluatedToDouble evaluate expr to Double deeply (success Double)") {
    val expr: Expr = 42.0.toExpr
    assertEquals(evaluator.evaluatedToDouble(op, expr, RuleCtx.empty(Map.empty)), Right(42.0))
  }

  test("evaluator.evaluatedToDouble evaluate expr to Double deeply (success function that evaluates to Double)") {
    val expr: Expr = Expr.RFunction("+", List(1.0.toExpr, 1.0.toExpr))
    assertEquals(evaluator.evaluatedToDouble(op, expr, RuleCtx.empty(Map.empty)), Right(2.0))
  }

  test("evaluator.evaluatedToDouble evaluate expr to Double deeply (failure)") {
    val expr: Expr = "hello".toExpr
    assertEquals(
      evaluator.evaluatedToDouble(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("Double", expr))),
    )
  }

  test("evaluator.evaluatedToString evaluate expr to String deeply (success String)") {
    val expr: Expr = "hello".toExpr
    assertEquals(evaluator.evaluatedToString(op, expr, RuleCtx.empty(Map.empty)), Right("hello"))
  }

  test("evaluator.evaluatedToString evaluate expr to String deeply (success function that evaluates to String)") {
    val expr: Expr = Expr.RFunction("+", List("hello".toExpr, " world".toExpr))
    assertEquals(evaluator.evaluatedToString(op, expr, RuleCtx.empty(Map.empty)), Right("hello world"))
  }

  test("evaluator.evaluatedToString evaluate expr to String deeply (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(
      evaluator.evaluatedToString(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("String", expr))),
    )
  }

  test("evaluator.evaluatedToList evaluate expr to List deeply (success List)") {
    val expr: Expr = List(1.toExpr, 2.toExpr, 3.toExpr).toExpr
    assertEquals(evaluator.evaluatedToList(op, expr, RuleCtx.empty(Map.empty)), Right(List(1.toExpr, 2.toExpr, 3.toExpr)))
  }

  test("evaluator.evaluatedToList evaluate expr to List deeply (success function that evaluates to List)") {
    val expr: Expr = "map([1,2,3], value()+1)".parsed
    assertEquals(evaluator.evaluatedToList(op, expr, RuleCtx.empty(Map.empty)), Right(List(2.toExpr, 3.toExpr, 4.toExpr)))
  }

  test("evaluator.evaluatedToList evaluate expr to List deeply (failure)") {
    val expr: Expr = 42.toExpr
    assertEquals(
      evaluator.evaluatedToList(op, expr, RuleCtx.empty(Map.empty)),
      Left(EvaluationError.OperationFailure(op, List(expr), FailureReason.InvalidArgumentType("List", expr))),
    )
  }

  test("evaluator.deepEvaluateFunctions evaluate expr until it's not a function anymore") {
    val expr: Expr = "+(1, +(2, 3))".parsed
    assertEquals(evaluator.deepEvaluateFunctions(expr, RuleCtx.empty(Map.empty)), Right(Expr.RInt(6)))
  }

  test("evaluator.deepEvaluateFunctionsAndLists evaluate expr until functions and list are evaluated") {
    val expr: Expr = "[[1, 2], [3, 4], [5, 6+1]]".parsed
    assertEquals(
      evaluator.deepEvaluateFunctionsAndLists(expr, RuleCtx.empty(Map.empty)),
      Right(
        Expr.RList(
          List(
            Expr.RList(List(Expr.RInt(1), Expr.RInt(2))),
            Expr.RList(List(Expr.RInt(3), Expr.RInt(4))),
            Expr.RList(List(Expr.RInt(5), Expr.RInt(7))),
          ),
        ),
      ),
    )
  }

  test("args.ints parse a list of expr to a list of ints (success)") {
    val args: List[Expr] = List(1.toExpr, 2.toExpr, 3.toExpr)
    assertEquals(args.ints, Right(List(1, 2, 3)))
  }

  test("args.ints parse a list of expr to a list of ints (failure)") {
    val args: List[Expr] = List(1.toExpr, "hello".toExpr, 3.toExpr)
    assertEquals(args.ints, Left(FailureReason.InvalidArgumentType("Int", "hello".toExpr)))
  }

  test("args.longs parse a list of expr to a list of longs (success)") {
    val args: List[Expr] = List(1L.toExpr, 2L.toExpr, 3L.toExpr)
    assertEquals(args.longs, Right(List(1L, 2L, 3L)))
  }

  test("args.longs parse a list of expr to a list of longs (failure)") {
    val args: List[Expr] = List(1L.toExpr, "hello".toExpr, 3L.toExpr)
    assertEquals(args.longs, Left(FailureReason.InvalidArgumentType("Long", "hello".toExpr)))
  }

  test("args.floats parse a list of expr to a list of floats (success)") {
    val args: List[Expr] = List(1.0f.toExpr, 2.0f.toExpr, 3.0f.toExpr)
    assertEquals(args.floats, Right(List(1.0f, 2.0f, 3.0f)))
  }

  test("args.floats parse a list of expr to a list of floats (failure)") {
    val args: List[Expr] = List(1.0f.toExpr, "hello".toExpr, 3.0f.toExpr)
    assertEquals(args.floats, Left(FailureReason.InvalidArgumentType("Float", "hello".toExpr)))
  }

  test("args.doubles parse a list of expr to a list of doubles (success)") {
    val args: List[Expr] = List(1.0.toExpr, 2.0.toExpr, 3.0.toExpr)
    assertEquals(args.doubles, Right(List(1.0, 2.0, 3.0)))
  }

  test("args.doubles parse a list of expr to a list of doubles (failure)") {
    val args: List[Expr] = List(1.0.toExpr, "hello".toExpr, 3.0.toExpr)
    assertEquals(args.doubles, Left(FailureReason.InvalidArgumentType("Double", "hello".toExpr)))
  }

  test("args.strings parse a list of expr to a list of strings (success)") {
    val args: List[Expr] = List("hello".toExpr, "world".toExpr)
    assertEquals(args.strings, Right(List("hello", "world")))
  }

  test("args.strings parse a list of expr to a list of strings (failure)") {
    val args: List[Expr] = List("hello".toExpr, 42.toExpr)
    assertEquals(args.strings, Left(FailureReason.InvalidArgumentType("String", 42.toExpr)))
  }

  test("args.booleans parse a list of expr to a list of booleans (success)") {
    val args: List[Expr] = List(true.toExpr, false.toExpr)
    assertEquals(args.booleans, Right(List(true, false)))
  }

  test("args.booleans parse a list of expr to a list of booleans (failure)") {
    val args: List[Expr] = List(true.toExpr, 42.toExpr)
    assertEquals(args.booleans, Left(FailureReason.InvalidArgumentType("Boolean", 42.toExpr)))
  }

}
