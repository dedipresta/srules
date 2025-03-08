package com.dedipresta.srules.evaluate.operators

import com.dedipresta.srules.evaluate.*

import cats.MonadError

object DefaultOperators:

  def all[F[_], Ctx](using
      monadError: MonadError[F, EvaluationError],
      ctxReader: UserContextReader[F, Ctx],
  ): Map[String, Operator[F, Ctx, EvaluationError]] = {
    val add       = Add[F, Ctx]()
    val sub       = Subtract[F, Ctx]()
    val div       = Divide[F, Ctx]()
    val mod       = Mod[F, Ctx]()
    val mul       = Multiply[F, Ctx]()
    val and       = And[F, Ctx]()
    val or        = Or[F, Ctx]()
    val not       = Not[F, Ctx]()
    val gt        = Gt[F, Ctx]()
    val gte       = Gte[F, Ctx]()
    val lt        = Lt[F, Ctx]()
    val lte       = Lte[F, Ctx]()
    val eq        = Equals[F, Ctx]()
    val ne        = NotEquals[F, Ctx]()
    val toLong    = ToLong[F, Ctx]()
    val toDouble  = ToDouble[F, Ctx]()
    val toFloat   = ToFloat[F, Ctx]()
    val toInt     = ToInt[F, Ctx]()
    val toString  = ToString[F, Ctx]()
    val toBoolean = ToBoolean[F, Ctx]()
    val pow       = Pow[F, Ctx]()

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
      "abs"       -> Abs[F, Ctx](),
      "acc"       -> Accumulator[F, Ctx](),
      "add"       -> add,
      "and"       -> and,
      "atIndex"   -> AtIndex[F, Ctx](),
      "ceil"      -> Ceil[F, Ctx](),
      "concat"    -> Concat[F, Ctx](),
      "contains"  -> Contains[F, Ctx](),
      "div"       -> div,
      "eval"      -> Eval[F, Ctx](),
      "exists"    -> Exists[F, Ctx](),
      "eq"        -> eq,
      "fail"      -> Fail[F, Ctx](),
      "filter"    -> Filter[F, Ctx](),
      "find"      -> Find[F, Ctx](),
      "floor"     -> Floor[F, Ctx](),
      "forAll"    -> ForAll[F, Ctx](),
      "gt"        -> gt,
      "gte"       -> gte,
      "if"        -> If[F, Ctx](),
      "index"     -> Index[F, Ctx](),
      "indexOf"   -> IndexOf[F, Ctx](),
      "isEmpty"   -> IsEmpty[F, Ctx](),
      "isNull"    -> IsNull[F, Ctx](),
      "lazyIf"    -> LazyIf[F, Ctx](),
      "lt"        -> lt,
      "lte"       -> lte,
      "map"       -> MapFn[F, Ctx](),
      "max"       -> Max[F, Ctx](),
      "mod"       -> mod,
      "min"       -> Min[F, Ctx](),
      "mul"       -> mul,
      "named"     -> Named[F, Ctx](),
      "ne"        -> ne,
      "nonEmpty"  -> NonEmpty[F, Ctx](),
      "not"       -> not,
      "notNull"   -> NotNull[F, Ctx](),
      "or"        -> or,
      "pow"       -> pow,
      "reduce"    -> Reduce[F, Ctx](),
      "round"     -> Round[F, Ctx](),
      "size"      -> Size[F, Ctx](),
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
      "value"     -> Value[F, Ctx](),
      "var"       -> Var[F, Ctx](),
    )
  }
