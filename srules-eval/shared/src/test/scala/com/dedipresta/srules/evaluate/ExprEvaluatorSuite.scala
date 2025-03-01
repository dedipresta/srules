package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.RuleCtx.DefaultContext
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*
import com.dedipresta.srules.given

import cats.syntax.all.*

import munit.*

final class ExprEvaluatorSuite extends FunSuite {

  given UserContextReader[Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[Map[String, Expr]] = new ExprEvaluatorImpl[Map[String, Expr]](DefaultOperators.all)

  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  val userCtx: Map[String, Expr] = Map[String, Expr](
    "simpleFunction"  -> "1+1".parsed,
    "unevaluatedList" -> "[1, $simpleFunction]".parsed,
  )

  test("evaluator.evaluate is not deep evaluating arguments") {
    assertEquals(
      SRules.parse("$simpleFunction").flatMap(evaluator.evaluate(_, userCtx)).map(_.show),
      Right("""(1+1)"""),
    )
  }

  test("evaluator.evaluate is not deep evaluating arguments") {
    assertEquals(
      SRules.parse("$unevaluatedList").flatMap(evaluator.evaluate(_, userCtx)).map(_.show),
      Right("""[1,var("simpleFunction")]"""),
    )
  }

  test("evaluator.deepEvaluateFunctions evaluate functions but not the lists") {
    assertEquals(
      SRules.parse("$simpleFunction").flatMap(evaluator.deepEvaluateFunctions(_, DefaultContext(userCtx, Map.empty))).map(_.show),
      Right("""2"""),
    )
  }

  test("evaluator.deepEvaluateFunctions evaluate functions and the lists") {
    assertEquals(
      SRules.parse("$unevaluatedList").flatMap(evaluator.deepEvaluateFunctions(_, DefaultContext(userCtx, Map.empty))).map(_.show),
      Right("""[1,var("simpleFunction")]"""),
    )
  }

  test("evaluator.deepEvaluateFunctionsAndLists evaluate functions and the lists") {
    assertEquals(
      SRules.parse("$simpleFunction").flatMap(evaluator.deepEvaluateFunctionsAndLists(_, DefaultContext(userCtx, Map.empty))).map(_.show),
      Right("""2"""),
    )
  }

  test("evaluator.deepEvaluateFunctionsAndLists evaluate functions and the lists") {
    assertEquals(
      SRules.parse("$unevaluatedList").flatMap(evaluator.deepEvaluateFunctionsAndLists(_, DefaultContext(userCtx, Map.empty))).map(_.show),
      Right("""[1,2]"""),
    )
  }

  test("evaluator.evaluateAll allow to evaluate and extract a value of type Int") {
    assertEquals(
      SRules.parse("(1+1)").flatMap(evaluator.evaluateAll(_, userCtx)),
      Right(Expr.RInt(2)),
    )
  }

  test("evaluator.evaluateAs[Int] allow to evaluate and extract a value of type Int") {
    assertEquals(
      SRules.parse("(1+1)").flatMap(evaluator.evaluateAllAs[Int](_, userCtx)),
      Right(2),
    )
  }

  test("evaluator.evaluateAs[Float] allow to evaluate and extract a value of type Float") {
    assertEquals(
      SRules.parse("(2.0f+3.2f)").flatMap(evaluator.evaluateAllAs[Float](_, userCtx)),
      Right(5.2f),
    )
  }

  test("evaluator.evaluateAs[Double] allow to evaluate and extract a value of type Double") {
    assertEquals(
      SRules.parse("(2.0+3.2)").flatMap(evaluator.evaluateAllAs[Double](_, userCtx)),
      Right(5.2),
    )
  }

  test("evaluator.evaluateAs[Long] allow to evaluate and extract a value of type Long") {
    assertEquals(
      SRules.parse("(2L+3L)").flatMap(evaluator.evaluateAllAs[Long](_, userCtx)),
      Right(5L),
    )
  }

  test("evaluator.evaluateAs[String] allow to evaluate and extract a value of type String") {
    assertEquals(
      SRules.parse("""("hello"+" "+"world")""").flatMap(evaluator.evaluateAllAs[String](_, userCtx)),
      Right("hello world"),
    )
  }

  test("evaluator.evaluateAs[Boolean] allow to evaluate and extract a value of type Boolean") {
    assertEquals(
      SRules.parse("""(1==1)""").flatMap(evaluator.evaluateAllAs[Boolean](_, userCtx)),
      Right(true),
    )
  }

  test("evaluator.evaluateAs[Int] fails if result is a double") {
    assertEquals(
      SRules.parse("(2.0+3.2)").flatMap(evaluator.evaluateAllAs[Int](_, userCtx)).map(_.show),
      Left(EvaluationError.OperationFailure("evaluateAs", List(Expr.RDouble(5.2d)), FailureReason.InvalidArgumentType("Int", Expr.RDouble(5.2d)))),
    )
  }

  test("evaluator.evaluateAsList[Int] allow to evaluate and extract a list of Int") {
    assertEquals(
      SRules.parse("[1, 2, 6-3]").flatMap(evaluator.evaluateAllAsList[Int](_, userCtx)),
      Right(List(1, 2, 3)),
    )
  }

  test("evaluator.evaluateAsList[Int] fails if result is not a list") {
    assertEquals(
      SRules.parse("1").flatMap(evaluator.evaluateAllAsList[Int](_, userCtx)).map(_.show),
      Left(EvaluationError.OperationFailure("evaluateAsList", List(Expr.RInt(1)), FailureReason.InvalidArgumentType("List", Expr.RInt(1)))),
    )
  }

}
