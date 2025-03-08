package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.RuleCtx.DefaultContext
import com.dedipresta.srules.evaluate.operators.*
import com.dedipresta.srules.evaluate.syntax.*

import munit.*

final class RuleCtxSuite extends FunSuite {

  type ErrorOr[A] = Either[EvaluationError, A]
  given UserContextReader[ErrorOr, Map[String, Expr]]          = UserContextReader.forMapExpr(notFoundToNull = true)
  val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

  extension (str: String) {
    def parsed: Expr = SRules.parse(str).getOrElse(throw new Exception(s"Failed to parse $str"))
  }

  test("RuleCtx.DefaultContext can be created") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    assertEquals(defaultContext.named, Map.empty)
  }

  test("RuleCtx.DefaultContext has no index value") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    assertEquals(defaultContext.indexValue, None)
  }

  test("RuleCtx.DefaultContext has no accumulator value") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    assertEquals(defaultContext.accumulatorValue, None)
  }

  test("RuleCtx.DefaultContext has no current value") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    assertEquals(defaultContext.currentValue, None)
  }

  test("RuleCtx.DefaultContext has no named value 'sample'") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    assertEquals(defaultContext.namedValue("sample"), None)
  }

  test("RuleCtx.DefaultContext allow adding multiple named values") {
    val defaultContext = RuleCtx.DefaultContext((), Map.empty)
    val values         = Map(
      "var1" -> 42L.toExpr,
      "var2" -> true.toExpr,
      "var3" -> "hello".toExpr,
    )
    val newContext     = values.foldLeft[RuleCtx[Unit]](defaultContext) { case (ctx, (name, value)) => ctx.addNamedValue(name, value) }
    assertEquals(newContext.namedValue("var1"), values.get("var1"))
    assertEquals(newContext.namedValue("var2"), values.get("var2"))
    assertEquals(newContext.namedValue("var3"), values.get("var3"))
  }

  test("RuleCtx.WithIndexedValue can be created from a DefaultContext") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = defaultContext.withIndexedValue(1, 42L.toExpr)
    assertEquals(indexedValue.indexValue, Some(1))
  }

  test("RuleCtx.WithIndexedValue can be created from a WithIndexedValue") {
    val defaultContext: DefaultContext[Unit]          = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit]  = defaultContext.withIndexedValue(1, 42L.toExpr)
    val indexedValue2: RuleCtx.WithIndexedValue[Unit] = indexedValue.withIndexedValue(2, 43L.toExpr)
    assertEquals(indexedValue2.indexValue, Some(2))
  }

  test("RuleCtx.WithIndexedValue can be created from a WithAccumulator") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit]   = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = accumulator.withIndexedValue(2, 43L.toExpr)
    assertEquals(indexedValue.indexValue, Some(2))
  }

  test("RuleCtx.WithIndexedValue has no accumulator value") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = defaultContext.withIndexedValue(1, 42L.toExpr)
    assertEquals(indexedValue.accumulatorValue, None)
  }

  test("RuleCtx.WithIndexedValue has a current value") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = defaultContext.withIndexedValue(1, 42L.toExpr)
    assertEquals(indexedValue.currentValue, Some(42L.toExpr))
  }

  test("RuleCtx.WithIndexedValue allow adding named values") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = defaultContext.withIndexedValue(1, 42L.toExpr)
    val values                                       = Map(
      "var1" -> 42L.toExpr,
      "var2" -> true.toExpr,
      "var3" -> "hello".toExpr,
    )
    val newContext                                   = values.foldLeft[RuleCtx[Unit]](indexedValue) { case (ctx, (name, value)) => ctx.addNamedValue(name, value) }
    assertEquals(newContext.namedValue("var1"), values.get("var1"))
    assertEquals(newContext.namedValue("var2"), values.get("var2"))
    assertEquals(newContext.namedValue("var3"), values.get("var3"))
  }

  test("RuleCtx.WithAccumulator can be created from a DefaultContext") {
    val defaultContext: DefaultContext[Unit]       = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit] = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    assertEquals(accumulator.indexValue, Some(1))
  }

  test("RuleCtx.WithAccumulator can be created from a WithIndexedValue") {
    val defaultContext: DefaultContext[Unit]         = RuleCtx.DefaultContext((), Map.empty)
    val indexedValue: RuleCtx.WithIndexedValue[Unit] = defaultContext.withIndexedValue(1, 42L.toExpr)
    val accumulator: RuleCtx.WithAccumulator[Unit]   = indexedValue.withAccumulator(2, 43L.toExpr, 0L.toExpr)
    assertEquals(accumulator.indexValue, Some(2))
  }

  test("RuleCtx.WithAccumulator can be created from a WithAccumulator") {
    val defaultContext: DefaultContext[Unit]        = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit]  = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    val accumulator2: RuleCtx.WithAccumulator[Unit] = accumulator.withAccumulator(2, 43L.toExpr, 0L.toExpr)
    assertEquals(accumulator2.indexValue, Some(2))
  }

  test("RuleCtx.WithAccumulator has an accumulator value") {
    val defaultContext: DefaultContext[Unit]       = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit] = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    assertEquals(accumulator.accumulatorValue, Some(0L.toExpr))
  }

  test("RuleCtx.WithAccumulator has a current value") {
    val defaultContext: DefaultContext[Unit]       = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit] = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    assertEquals(accumulator.currentValue, Some(42L.toExpr))
  }

  test("RuleCtx.WithAccumulator allow adding named values") {
    val defaultContext: DefaultContext[Unit]       = RuleCtx.DefaultContext((), Map.empty)
    val accumulator: RuleCtx.WithAccumulator[Unit] = defaultContext.withAccumulator(1, 42L.toExpr, 0L.toExpr)
    val values                                     = Map(
      "var1" -> 42L.toExpr,
      "var2" -> true.toExpr,
      "var3" -> "hello".toExpr,
    )
    val newContext                                 = values.foldLeft[RuleCtx[Unit]](accumulator) { case (ctx, (name, value)) => ctx.addNamedValue(name, value) }
    assertEquals(newContext.namedValue("var1"), values.get("var1"))
    assertEquals(newContext.namedValue("var2"), values.get("var2"))
    assertEquals(newContext.namedValue("var3"), values.get("var3"))
  }

}
