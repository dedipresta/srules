package com.dedipresta.srules

import munit.*

final class ParserSuite extends FunSuite {

  import Expr.*

  test("parse null value") {
    assertEquals(Parser.nullValue.parseAll("null"), Right(RNull))
  }

  test("parse string to boolean") {
    assertEquals(Parser.parseBoolean.parseAll("true"), Right(true))
    assertEquals(Parser.parseBoolean.parseAll("false"), Right(false))
  }

  test("parse string to unsigned digits string") {
    assertEquals(Parser.parseUnsignedDigits.parseAll("123"), Right("123"))
  }

  test("parse string to signed digits string") {
    assertEquals(Parser.parseSignedDigits.parseAll("123"), Right("123"))
    assertEquals(Parser.parseSignedDigits.parseAll("+123"), Right("+123"))
    assertEquals(Parser.parseSignedDigits.parseAll("-123"), Right("-123"))
  }

  test("parse an integer value") {
    assertEquals(Parser.integerValue.parseAll("123"), Right(RInt(123)))
    assertEquals(Parser.integerValue.parseAll("+123"), Right(RInt(123)))
    assertEquals(Parser.integerValue.parseAll("-123"), Right(RInt(-123)))
  }

  test("parse a long value") {
    assertEquals(Parser.longValue.parseAll("123L"), Right(RLong(123)))
    assertEquals(Parser.longValue.parseAll("+123L"), Right(RLong(123)))
    assertEquals(Parser.longValue.parseAll("-123L"), Right(RLong(-123)))
  }

  test("parse a double value") {
    assertEquals(Parser.doubleValue.parseAll("123d"), Right(RDouble(123d)))
    assertEquals(Parser.doubleValue.parseAll("123.456d"), Right(RDouble(123.456d)))
    assertEquals(Parser.doubleValue.parseAll("123.456"), Right(RDouble(123.456)))
    assertEquals(Parser.doubleValue.parseAll("+123.456"), Right(RDouble(123.456)))
    assertEquals(Parser.doubleValue.parseAll("-123.456"), Right(RDouble(-123.456)))
  }

  test("parse a float value") {
    assertEquals(Parser.floatValue.parseAll("123f"), Right(RFloat(123f)))
    assertEquals(Parser.floatValue.parseAll("123.456f"), Right(RFloat(123.456f)))
    assertEquals(Parser.floatValue.parseAll("+123.456f"), Right(RFloat(123.456f)))
    assertEquals(Parser.floatValue.parseAll("-123.456f"), Right(RFloat(-123.456f)))
  }

  test("parse a string value") {
    assertEquals(Parser.stringValue.parseAll("\"hello\""), Right(RString("hello")))
  }

  test("parse a number value") {
    assertEquals(Parser.numericValue.parseAll("123"), Right(RInt(123)))
    assertEquals(Parser.numericValue.parseAll("123L"), Right(RLong(123L)))
    assertEquals(Parser.numericValue.parseAll("123.456"), Right(RDouble(123.456)))
  }

  test("parse a value") {
    assertEquals(Parser.parser.parseAll("null"), Right(RNull))
    assertEquals(Parser.parser.parseAll("true"), Right(RBoolean(true)))
    assertEquals(Parser.parser.parseAll("123"), Right(RInt(123)))
    assertEquals(Parser.parser.parseAll("123L"), Right(RLong(123L)))
    assertEquals(Parser.parser.parseAll("123.456"), Right(RDouble(123.456)))
    assertEquals(Parser.parser.parseAll("\"hello\""), Right(RString("hello")))
  }

  test("parse a variable value") {
    assertEquals(Parser.varValue.parseAll("$var"), Right(RFunction("var", RString("var"))))
  }

  test("parse a variable value (expanded format)") {
    assertEquals(Parser.parser.parseAll("${my.var}"), Right(RFunction("var", RString("my.var"))))
  }

  test("parse a variable value (expanded format and whitespace)") {
    assertEquals(Parser.parser.parseAll("${ my.var }"), Right(RFunction("var", RString("my.var"))))
  }

  test("parse an addition expression (infix)") {
    assertEquals(Parser.parser.parseAll("1 + 2"), Right(RFunction("+", RInt(1), RInt(2))))
  }

  test("parse an addition expression (function mode)") {
    assertEquals(Parser.parser.parseAll("+(1,2)"), Right(RFunction("+", RInt(1), RInt(2))))
  }

  test("parse a subtraction expression infix") {
    assertEquals(Parser.parser.parseAll("1-2"), Right(RFunction("-", RInt(1), RInt(2))))
  }

  test("parse a subtraction expression (function mode)") {
    assertEquals(Parser.parser.parseAll("-(1,2)"), Right(RFunction("-", RInt(1), RInt(2))))
  }

  test("parse a multiplication expression (infix)") {
    assertEquals(Parser.parser.parseAll("1*2"), Right(RFunction("*", RInt(1), RInt(2))))
  }

  test("parse a multiplication expression (function mode)") {
    assertEquals(Parser.parser.parseAll("*(1,2)"), Right(RFunction("*", RInt(1), RInt(2))))
  }

  test("parse a division expression (infix)") {
    assertEquals(Parser.parser.parseAll("1/2"), Right(RFunction("/", RInt(1), RInt(2))))
  }

  test("parse a division expression (function mode)") {
    assertEquals(Parser.parser.parseAll("/(1,2)"), Right(RFunction("/", RInt(1), RInt(2))))
  }

  test("parse a modulus expression (infix)") {
    assertEquals(Parser.parser.parseAll("1%2"), Right(RFunction("%", RInt(1), RInt(2))))
  }

  test("parse a modulus expression (function mode)") {
    assertEquals(Parser.parser.parseAll("%(1,2)"), Right(RFunction("%", RInt(1), RInt(2))))
  }

  test("parse multiple addition expressions (infix)") {
    assertEquals(
      Parser.parser.parseAll("1+2+3"),
      Right(RFunction("+", RFunction("+", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("parse multiple addition expressions (function mode)") {
    assertEquals(
      Parser.parser.parseAll("+(1,2,3)"),
      Right(RFunction("+", RInt(1), RInt(2), RInt(3))),
    )
  }

  test("parse multiple subtraction expressions") {
    assertEquals(
      Parser.parser.parseAll("1-2-3"),
      Right(RFunction("-", RFunction("-", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("parse multiple multiplication expressions") {
    assertEquals(
      Parser.parser.parseAll("1*2*3"),
      Right(RFunction("*", RFunction("*", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("parse multiple division expressions") {
    assertEquals(
      Parser.parser.parseAll("1/2/3"),
      Right(RFunction("/", RFunction("/", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("parse multiple modulus expressions") {
    assertEquals(
      Parser.parser.parseAll("1%2%3"),
      Right(RFunction("%", RFunction("%", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("parse addition and subtraction expressions") {
    assertEquals(
      Parser.parser.parseAll("1+2-3"),
      Right(RFunction("-", RFunction("+", RInt(1), RInt(2)), RInt(3))),
    )
  }

  test("Parser.parser parse a variable value") {
    assertEquals(Parser.parser.parseAll("$var1"), Right(RFunction("var", RString("var1"))))
  }

  test("parse addition between parenthesis") {
    assertEquals(Parser.parser.parseAll("(1+2)"), Right(RFunction("+", RInt(1), RInt(2))))
  }

  test("parse multiplication between parenthesis") {
    assertEquals(Parser.parser.parseAll("(1*2)"), Right(RFunction("*", RInt(1), RInt(2))))
  }

  test("parse additions in parenthesis") {
    assertEquals(
      Parser.parser.parseAll("(1+2)+(3+4)"),
      Right(
        RFunction("+", RFunction("+", RInt(1), RInt(2)), RFunction("+", RInt(3), RInt(4))),
      ),
    )
  }

  test("parse complex formula in nested parenthesis") {
    assertEquals(
      Parser.parser.parseAll("((5+8)*1+((2+4)*3))"),
      Right(
        RFunction(
          "+",
          RFunction("*", RFunction("+", RInt(5), RInt(8)), RInt(1)),
          RFunction("*", RFunction("+", RInt(2), RInt(4)), RInt(3)),
        ),
      ),
    )
  }

  test("parse a function to a function value") {
    assertEquals(Parser.parser.parseAll("func()"), Right(RFunction("func", List.empty)))
    assertEquals(Parser.parser.parseAll("func(1)"), Right(RFunction("func", List(RInt(1)))))
    assertEquals(Parser.parser.parseAll("func(1,2)"), Right(RFunction("func", List(RInt(1), RInt(2)))))
  }

  test("parse a complex expression") {
    assertEquals(
      Parser.parser.parseAll("0.5*(floor(1.5-random(0,8))+ceil($var2*2.5))"),
      Right(
        RFunction(
          "*",
          RDouble(0.5),
          RFunction(
            "+",
            RFunction(
              "floor",
              List(
                RFunction("-", RDouble(1.5), RFunction("random", List(RInt(0), RInt(8)))),
              ),
            ),
            RFunction("ceil", List(RFunction("*", RFunction("var", RString("var2")), RDouble(2.5)))),
          ),
        ),
      ),
    )
  }

  test("parse a and expression") {
    assertEquals(
      Parser.parser.parseAll("true&&false"),
      Right(RFunction("&&", RBoolean(true), RBoolean(false))),
    )
  }

  test("parse a or expression") {
    assertEquals(
      Parser.parser.parseAll("true||false"),
      Right(RFunction("||", RBoolean(true), RBoolean(false))),
    )
  }

  test("parse a not expression") {
    assertEquals(
      Parser.parser.parseAll("!true"),
      Right(RFunction("!", RBoolean(true))),
    )
  }

  test("parse a not expression in parenthesis") {
    assertEquals(
      Parser.parser.parseAll("!(true)"),
      Right(RFunction("!", RBoolean(true))),
    )
  }

  test("parse a complex expression with and, or, not") {
    assertEquals(
      Parser.parser.parseAll("true&&false||!true"),
      Right(
        RFunction(
          "||",
          RFunction("&&", RBoolean(true), RBoolean(false)),
          RFunction("!", RBoolean(true)),
        ),
      ),
    )
  }

  test("parse a complex expression with and, or, not (2)") {
    assertEquals(
      Parser.parser.parseAll("true||false&&!true"),
      Right(
        RFunction(
          "||",
          RBoolean(true),
          RFunction("&&", RBoolean(false), RFunction("!", RBoolean(true))),
        ),
      ),
    )
  }

  test("parse a boolean expression based on integers comparison") {
    assertEquals(
      Parser.parser.parseAll("1<2"),
      Right(RFunction("<", RInt(1), RInt(2))),
    )
    assertEquals(
      Parser.parser.parseAll("1<=2"),
      Right(RFunction("<=", RInt(1), RInt(2))),
    )
    assertEquals(
      Parser.parser.parseAll("1>2"),
      Right(RFunction(">", RInt(1), RInt(2))),
    )
    assertEquals(
      Parser.parser.parseAll("1>=2"),
      Right(RFunction(">=", RInt(1), RInt(2))),
    )
    assertEquals(
      Parser.parser.parseAll("1==2"),
      Right(RFunction("==", RInt(1), RInt(2))),
    )
    assertEquals(
      Parser.parser.parseAll("1!=2"),
      Right(RFunction("!=", RInt(1), RInt(2))),
    )
  }

  test("parse a complex expression with parenthesis and whitespace") {
    val expr        = "1 + (2*3) - 3*$var1"
    val mult1       = RFunction("*", RInt(2), RInt(3))
    val mult2       = RFunction("*", RInt(3), RFunction("var", RString("var1")))
    val add         = RFunction("+", RInt(1), mult1)
    val expectation = RFunction("-", add, mult2)
    assertEquals(
      Parser.parser.parseAll("1 + (2*3) - 3*$var1"),
      Right(expectation),
    )
  }

  test("parse a logical expression that should be short-circuited") {
    assertEquals(
      Parser.parser.parseAll("true && (1/0)"),
      Right(RFunction("&&", RBoolean(true), RFunction("/", RInt(1), RInt(0)))),
    )
    assertEquals(
      Parser.parser.parseAll("true || (1/0)"),
      Right(RFunction("||", RBoolean(true), RFunction("/", RInt(1), RInt(0)))),
    )
  }

  test("parse a not equal expression with multiple arguments (empty)") {
    assertEquals(
      Parser.parser.parseAll("!=()"),
      Right(RFunction("!=", List.empty)),
    )
  }

  test("parse an array of integers") {
    assertEquals(
      Parser.parser.parseAll("[1,2,3]"),
      Right(RList(List(RInt(1), RInt(2), RInt(3)))),
    )
  }

  test("parse an array of integers with whitespace") {
    assertEquals(
      Parser.parser.parseAll("[ 1 , 2 , 3 ]"),
      Right(RList(List(RInt(1), RInt(2), RInt(3)))),
    )
  }

  test("parse an array of sub expressions") {
    assertEquals(
      Parser.parser.parseAll("[1,2+3,4*5]"),
      Right(RList(List(RInt(1), RFunction("+", RInt(2), RInt(3)), RFunction("*", RInt(4), RInt(5))))),
    )
  }

  test("parse an expression with nested functions") {
    assertEquals(
      Parser.parser.parseAll("""named("i", $var1, if(isNull(named("i")), -1, named("i")+1))"""),
      Right(
        RFunction(
          "named",
          RString("i"),
          RFunction(
            "var",
            RString("var1"),
          ),
          RFunction(
            "if",
            RFunction(
              "isNull",
              RFunction("named", RString("i")),
            ),
            RInt(-1),
            RFunction(
              "+",
              RFunction("named", RString("i")),
              RInt(1),
            ),
          ),
        ),
      ),
    )
  }

}
