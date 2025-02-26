package com.dedipresta.srules

sealed trait Expr
object Expr {

  case object RNull                                    extends Expr
  case class RBoolean(value: Boolean)                  extends Expr
  case class RInt(value: Int)                          extends Expr
  case class RLong(value: Long)                        extends Expr
  case class RDouble(value: Double)                    extends Expr
  case class RFloat(value: Float)                      extends Expr
  case class RString(value: String)                    extends Expr
  case class RList(value: List[Expr])                  extends Expr
  case class RFunction(name: String, args: List[Expr]) extends Expr // Rename ROp

  extension (f: RFunction.type) {
    def apply(name: String, args: Expr*): RFunction =
      RFunction(name, args.toList)
  }
}
