package com.dedipresta.srules

import cats.parse.{Parser => CatsParser, *}
import cats.parse.Parser.*
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.digit
import cats.parse.Rfc5234.dquote

private[srules] object Parser {

  import Expr.*

  // whitespaces
  val ws: CatsParser[Unit] = charIn(" \t\n\r").void
  val ws0: Parser0[Unit]   = ws.rep0.void

  val parseNull: CatsParser[Unit]             = string("null").void
  val parseBoolean: CatsParser[Boolean]       = string("true").as(true) | string("false").as(false)
  val parseUnsignedDigits: CatsParser[String] = digit.rep.string
  val parseSignedDigits: CatsParser[String]   = (charIn("+-").?.string.with1 ~ parseUnsignedDigits).map(_ + _)
  val parseInteger: CatsParser[Int]           = parseSignedDigits.map(_.toInt)
  val parseLong: CatsParser[Long]             = (parseSignedDigits ~ charIn("lL")).map(_._1.toLong)

  // floating points may or may not have decimal part
  // if no decimal part, a f or F is needed for float, d or D for double
  // if decimal part, a f or F is needed for float, d or D is optional for double
  val parseDecimalPart: CatsParser[String]   = (char('.') ~ parseUnsignedDigits).string
  val parseFloatingPoint: CatsParser[String] = (parseSignedDigits ~ parseDecimalPart.?).map((a, b) => a + b.getOrElse(""))

  // float
  val parseFloat: CatsParser[Float]           = (parseFloatingPoint <* charIn("fF")).map(_.toFloat)
  // double
  val parseDoubleNoHint: CatsParser[Double]   = (parseSignedDigits ~ parseDecimalPart).string.map(_.toDouble) // no final d or D
  val parseDoubleWithHint: CatsParser[Double] = (parseFloatingPoint <* charIn("dD")).map(_.toDouble)          // final d or D
  val parseDouble: CatsParser[Double]         = parseDoubleWithHint.backtrack | parseDoubleNoHint

  val stringChar: CatsParser[Char]          = CatsParser.oneOf(List((char('\\') ~ char('"')).as('"'), charWhere(_ != '"')))
  val innerQuotes: Parser0[String]          = stringChar.rep0.string
  val parseQuotedString: CatsParser[String] = dquote *> innerQuotes <* dquote

  // parse variable names format: $varName_1 or ${can.be.dotted.var.name_1}
  val varFirstChar: CatsParser[String]      = (alpha | charIn("_")).string
  val varSubName: CatsParser[String]        = (varFirstChar ~ (alpha | digit | charIn("_")).rep0).string
  val dottedVarName: CatsParser[String]     = (varSubName ~ (char('.') ~ varSubName).rep0).string
  val inBracketsVarName: CatsParser[String] = char('{') *> dottedVarName.surroundedBy(ws0) <* char('}')
  val varName: CatsParser[String]           = varSubName | inBracketsVarName
  val parseVar: CatsParser[String]          = char('$') *> varName

  val nullValue: CatsParser[RNull.type]  = parseNull.as(Expr.RNull)
  val varValue: CatsParser[RFunction]    = parseVar.map(name => Expr.RFunction("var", List(Expr.RString(name))))
  val booleanValue: CatsParser[RBoolean] = parseBoolean.map(Expr.RBoolean.apply)
  val integerValue: CatsParser[RInt]     = parseInteger.map(Expr.RInt.apply)
  val longValue: CatsParser[RLong]       = parseLong.map(Expr.RLong.apply)
  val doubleValue: CatsParser[RDouble]   = parseDouble.map(Expr.RDouble.apply)
  val floatValue: CatsParser[RFloat]     = parseFloat.map(Expr.RFloat.apply)
  val stringValue: CatsParser[RString]   = parseQuotedString.map(Expr.RString.apply)

  // arithmetic operators and expressions
  // ORDER IS VERY IMPORTANT TO ENSURE BACKTRACKING WORKS
  val lowerPrecedence: List[String]           = List("+", "-", "||", "<", "<=", ">", ">=", "==", "!=").sortBy(_.length).reverse
  val lowerPrecedenceOps: CatsParser[String]  = oneOf(lowerPrecedence.map(op => string(op).as(op)))
  val higherPrecedence: List[String]          = List("*", "/", "%", "&&").sortBy(_.length).reverse
  val higherPrecedenceOps: CatsParser[String] = oneOf(higherPrecedence.map(op => string(op).as(op))).string

  val numericValue: CatsParser[Expr] = longValue.backtrack | floatValue.backtrack | doubleValue.backtrack | integerValue

  def infixToFunctionValue(s: String, left: Expr, right: Expr): RFunction = RFunction(s, List(left, right))

  val parser: CatsParser[Expr] = CatsParser.recursive[Expr] { recurse =>

    def array: CatsParser[Expr] = (char('[') *> recurse.repSep0(char(',')) <* char(']')).map(Expr.RList.apply)

    def parens: CatsParser[Expr] = char('(') *> recurse <* char(')')

    // function parsing format: function_name(arg1, arg2, arg3, ...)
    val firstCharFn: CatsParser[String]      = (alpha | charIn("_!><&=*/*+-%")).string
    val functionName: CatsParser[String]     = (firstCharFn ~ (firstCharFn | digit).rep0).string
    val functionValue: CatsParser[RFunction] =
      (functionName ~ char('(') ~ (recurse.repSep0(char(',')) <* char(')'))).map { case ((name, _), args) => Expr.RFunction(name, args) }

    val value: CatsParser[Expr] =
      functionValue.backtrack | array.backtrack |
        nullValue.backtrack | booleanValue.backtrack | numericValue.backtrack | varValue.backtrack | stringValue

    // unary operators
    // string("-") is not added since it is either parsed with a number or in the subtraction operator
    // here we do not recurse but are parsing a simple value not a full expression
    // when a full expression is expected (e.g with parens) this is handled by the function parsing
    def parseUnary: CatsParser[Expr] = (oneOf(List(string("!"))).string ~ value).map((op, expr) => RFunction(op, List(expr)))

    def expressionRhs: CatsParser[(String, Expr)] = lowerPrecedenceOps ~ term

    def expression: CatsParser[Expr] =
      (term ~ expressionRhs.rep0).map((left, right) => right.foldLeft(left) { case (acc, (c, next)) => infixToFunctionValue(c, acc, next) })

    def termRhs: CatsParser[(String, Expr)] = higherPrecedenceOps ~ factor

    def term: CatsParser[Expr] =
      (factor ~ termRhs.rep0).map((left, right) => right.foldLeft(left) { case (acc, (c, next)) => infixToFunctionValue(c, acc, next) })

    def factor: CatsParser[Expr] = (parens | value | parseUnary).surroundedBy(ws0)

    expression

  }

}
