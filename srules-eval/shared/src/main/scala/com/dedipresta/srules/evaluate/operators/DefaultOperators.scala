package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.evaluate.EvaluationError
import com.dedipresta.srules.evaluate.Operator
import com.dedipresta.srules.evaluate.UserContextReader

object DefaultOperators:

  def all[Ctx: UserContextReader]: Map[String, Operator[Ctx, EvaluationError]] = {
    val add       = Add[Ctx]()
    val sub       = Subtract[Ctx]()
    val div       = Divide[Ctx]()
    val mod       = Mod[Ctx]()
    val mul       = Multiply[Ctx]()
    val and       = And[Ctx]()
    val or        = Or[Ctx]()
    val not       = Not[Ctx]()
    val gt        = Gt[Ctx]()
    val gte       = Gte[Ctx]()
    val lt        = Lt[Ctx]()
    val lte       = Lte[Ctx]()
    val eq        = Equals[Ctx]()
    val ne        = NotEquals[Ctx]()
    val toLong    = ToLong[Ctx]()
    val toDouble  = ToDouble[Ctx]()
    val toFloat   = ToFloat[Ctx]()
    val toInt     = ToInt[Ctx]()
    val toString  = ToString[Ctx]()
    val toBoolean = ToBoolean[Ctx]()
    val pow       = Pow[Ctx]()

    Map(
      "+"         -> add,
      "-"         -> sub,
      "/"         -> div,
      "%"         -> mod,
      "*"         -> mul,
      "&&"        -> and,
      "||"        -> or,
      ">"         -> gt,
      ">="        -> gte,
      "<"         -> lt,
      "<="        -> lte,
      "=="        -> eq,
      "!="        -> ne,
      "!"         -> not,
      "^"         -> pow,
      "abs"       -> Abs[Ctx](),
      "acc"       -> Accumulator[Ctx](),
      "add"       -> add,
      "and"       -> and,
      "atIndex"   -> AtIndex[Ctx](),
      "ceil"      -> Ceil[Ctx](),
      "contains"  -> Contains[Ctx](),
      "div"       -> div,
      "eval"      -> Eval[Ctx](),
      "exists"    -> Exists[Ctx](),
      "eq"        -> eq,
      "fail"      -> Fail[Ctx](),
      "filter"    -> Filter[Ctx](),
      "find"      -> Find[Ctx](),
      "floor"     -> Floor[Ctx](),
      "forAll"    -> ForAll[Ctx](),
      "gt"        -> gt,
      "gte"       -> gte,
      "if"        -> If[Ctx](),
      "index"     -> Index[Ctx](),
      "indexOf"   -> IndexOf[Ctx](),
      "isEmpty"   -> IsEmpty[Ctx](),
      "isNull"    -> IsNull[Ctx](),
      "lazyIf"    -> LazyIf[Ctx](),
      "lt"        -> lt,
      "lte"       -> lte,
      "map"       -> MapFn[Ctx](),
      "max"       -> Max[Ctx](),
      "mod"       -> mod,
      "min"       -> Min[Ctx](),
      "mul"       -> mul,
      "named"     -> Named[Ctx](),
      "ne"        -> ne,
      "nonEmpty"  -> NonEmpty[Ctx](),
      "not"       -> not,
      "notNull"   -> NotNull[Ctx](),
      "or"        -> or,
      "pow"       -> pow,
      "reduce"    -> Reduce[Ctx](),
      "round"     -> Round[Ctx](),
      "size"      -> Size[Ctx](),
      "sub"       -> sub,
      "toBoolean" -> toBoolean,
      "bool"      -> toBoolean,
      "toDouble"  -> toDouble,
      "double"    -> toDouble,
      "toFloat"   -> toFloat,
      "float"     -> toFloat,
      "toInt"     -> toInt,
      "int"       -> toInt,
      "toLong"    -> toLong,
      "long"      -> toLong,
      "toString"  -> toString,
      "string"    -> toString,
      "value"     -> Value[Ctx](),
      "var"       -> Var[Ctx](),
    )
  }
