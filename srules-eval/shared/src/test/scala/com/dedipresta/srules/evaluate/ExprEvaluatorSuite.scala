package com.dedipresta.srules.evaluate

import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

import munit.*

final class ExprEvaluatorSuite extends FunSuite {

  type Ctx = Map[String, Any]

  private val operators = {
    val not = Not[Ctx]()
    val and = And[Ctx]()
    // lt, lte, gt, gte could be refactored since they are all similar // TODO check Gt since it has been partially refactored
    val lt  = Lt[Ctx]()
    val lte = Lte[Ctx]()
    val gt  = Gt[Ctx]()
    val gte = Gte[Ctx]()

    Map[String, Operator[Ctx, EvaluationError]](
      "+"         -> Add(),
      "-"         -> Subtract(),
      "*"         -> Multiply(),
      "/"         -> Divide(),
      "%"         -> Mod(),
      "var"       -> VarFromMapAny(),
      "&&"        -> and,
      "and"       -> and, // alias
      "||"        -> Or(),
      "!"         -> not,
      "not"       -> not, // alias
      "=="        -> Equal(),
      "!="        -> NotEqual(),
      "exists"    -> Exists(),
      "<"         -> lt,
      "lt"        -> lt,  // alias
      "<="        -> lte,
      "lte"       -> lte, // alias
      ">"         -> gt,
      "gt"        -> gt,  // alias
      ">="        -> gte,
      "gte"       -> gte, // alias
      "toInt"     -> ToInt(),
      "toFloat"   -> ToFloat(),
      "toDouble"  -> ToDouble(),
      "toLong"    -> ToLong(),
      "toBoolean" -> ToBoolean(),
      "toString"  -> ToString(),
      "floor"     -> Floor(),
      "ceil"      -> Ceil(),
      "max"       -> Max(),
      "min"       -> Min(),
      "map"       -> MapFn(),
      "filter"    -> FilterFn(),
      "reduce"    -> ReduceFn(),
      "if"        -> If(),
    )
  }

  val evaluator = new ExprEvaluatorImpl[Ctx](operators)


}
