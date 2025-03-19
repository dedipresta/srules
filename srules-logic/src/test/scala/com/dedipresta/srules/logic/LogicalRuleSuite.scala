package com.dedipresta.srules.logic

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*
import com.dedipresta.srules.logic.LogicalRule.*

import cats.data.NonEmptyList

import munit.*

final class LogicalRuleSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]              = UserContextReader.forMapExpr(notFoundToNull = true)
  val exprEvaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  val evaluator = new LogicalEvaluatorImpl[ErrorOr, Map[String, Expr]](exprEvaluator)

  test("LogicalEvaluator.evaluate should allow to evaluate a simple rule to a boolean value (true)") {
    val rule = SimpleRule("some rule name", true.toExpr)
    assertEquals(
      evaluator.evaluate(rule, Map.empty),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluateWithReport should allow to evaluate a simple rule to a report (true)") {
    val rule = SimpleRule("some rule name", true.toExpr)
    assertEquals(
      evaluator.evaluateWithReport(rule, Map.empty),
      Right(Report("some rule name", true, None, Nil)),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate a simple rule to a boolean value (false)") {
    val rule = SimpleRule("some rule name", false.toExpr)
    assertEquals(
      evaluator.evaluate(rule, Map.empty),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluateWithReport should allow to evaluate a simple rule to a report (false)") {
    val rule = SimpleRule("some rule name", false.toExpr)
    assertEquals(
      evaluator.evaluateWithReport(rule, Map.empty),
      Right(Report("some rule name", false, None, Nil)),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate a simple rule to a boolean value (evaluated to true)") {
    val rule = SimpleRule("some rule name", SRules.parseOrThrow("$a > 0"))
    assertEquals(
      evaluator.evaluate(rule, Map("a" -> 1.toExpr)),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate a simple rule to a boolean value (evaluated to false)") {
    val rule = SimpleRule("some rule name", SRules.parseOrThrow("$a > 0"))
    assertEquals(
      evaluator.evaluate(rule, Map("a" -> (-1).toExpr)),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (allOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.AllOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> 1.toExpr)),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluateWithReport should allow to evaluate combined rules to a report (allOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.AllOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluateWithReport(rule, Map("a" -> 1.toExpr)),
      Right(
        Report(
          "some rule name",
          true,
          Some(LogicalCombinator.AllOf),
          List(
            Report("some rule name", true, None, Nil),
            Report("some rule name", true, None, Nil),
          ),
        ),
      ),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (allOf combinator false)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.AllOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> (-1).toExpr)),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (oneOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.OneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", false.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> 1.toExpr)),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluateWithReport should allow to evaluate combined rules to a report (oneOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.OneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluateWithReport(rule, Map("a" -> 1.toExpr)),
      Right(
        Report(
          "some rule name",
          true,
          Some(LogicalCombinator.OneOf),
          List(
            Report("some rule name", true, None, Nil),
            Report("some rule name", true, None, Nil),
          ),
        ),
      ),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (oneOf combinator false)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.OneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", false.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> (-1).toExpr)),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (noneOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.NoneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", false.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> (-1).toExpr)),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluateWithReport should allow to evaluate combined rules to a report (noneOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.NoneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", false.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluateWithReport(rule, Map("a" -> (-1).toExpr)),
      Right(
        Report(
          "some rule name",
          true,
          Some(LogicalCombinator.NoneOf),
          List(
            Report("some rule name", false, None, Nil),
            Report("some rule name", false, None, Nil),
          ),
        ),
      ),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate combined rules to a boolean value (noneOf combinator true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.NoneOf,
      NonEmptyList.of(
        SimpleRule("some rule name", false.toExpr),
        SimpleRule("some rule name", SRules.parseOrThrow("$a > 0")),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map("a" -> 1.toExpr)),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate nested logical expression (true)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.AllOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        CombinedRules(
          "some rule name",
          LogicalCombinator.OneOf,
          NonEmptyList.of(
            SimpleRule("some rule name", false.toExpr),
            CombinedRules(
              "some rule name",
              LogicalCombinator.AllOf,
              NonEmptyList.of(
                SimpleRule("some rule name", true.toExpr),
              ),
            ),
          ),
        ),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map.empty),
      Right(true),
    )
  }

  test("LogicalEvaluator.evaluate should allow to evaluate nested logical expression (false)") {
    val rule = CombinedRules(
      "some rule name",
      LogicalCombinator.AllOf,
      NonEmptyList.of(
        SimpleRule("some rule name", true.toExpr),
        CombinedRules(
          "some rule name",
          LogicalCombinator.OneOf,
          NonEmptyList.of(
            SimpleRule("some rule name", false.toExpr),
            CombinedRules(
              "some rule name",
              LogicalCombinator.AllOf,
              NonEmptyList.of(
                SimpleRule("some rule name", false.toExpr),
              ),
            ),
          ),
        ),
      ),
    )

    assertEquals(
      evaluator.evaluate(rule, Map.empty),
      Right(false),
    )
  }

  test("LogicalEvaluator.evaluate should forward the error") {
    val rule = SimpleRule("some rule name", "not a boolean".toExpr)
    assertEquals(
      evaluator.evaluate(rule, Map.empty).isLeft,
      true,
    )
  }

}
