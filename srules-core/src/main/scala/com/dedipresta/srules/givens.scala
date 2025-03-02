package com.dedipresta.srules

import cats.Show

private val infixOperators =
  Set(
    "+",
    "-",
    "*",
    "/",
    "%",
    "<",
    "<=",
    ">",
    ">=",
    "==",
    "!=",
    "&&",
    "||",
  )

given Show[Expr] = new Show[Expr] {

  private def renderFloat(f: Float): String =
    val s = f.toString
    if s.contains('.') then s"${s}f"
    // $COVERAGE-OFF$ scala.js
    else s"$s.0f"
    // $COVERAGE-ON$

  private def renderDouble(d: Double): String =
    val s = d.toString
    if s.contains('.') then s"${s}d"
    // $COVERAGE-OFF$ scala.js
    else s"$s.0d"
    // $COVERAGE-ON$

  def show(e: Expr): String =
    e match
      case Expr.RInt(i)         => i.toString
      case Expr.RLong(l)        => l.toString
      case Expr.RFloat(f)       => renderFloat(f)
      case Expr.RDouble(d)      => renderDouble(d)
      case Expr.RString(s)      => s"\"$s\""
      case Expr.RBoolean(b)     => b.toString
      case Expr.RNull           => "null"
      case Expr.RList(l)        => s"[${l.map(e => show(e)).mkString(",")}]"
      case Expr.RFunction(n, a) =>
        if (a.size == 2 && infixOperators.contains(n))
          s"(${show(a.head)}$n${show(a(1))})" // infix operator with no known precedence
        else
          s"$n(${a.map(e => show(e)).mkString(",")})"

}
