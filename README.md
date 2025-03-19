# SRules

[![Scala 3](https://img.shields.io/badge/Scala-3-blue)](https://www.scala-lang.org/)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.18.0.svg)](https://www.scala-js.org)
![version](https://img.shields.io/badge/semver-1.0.0-success.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-blueviolet.svg)](https://opensource.org/licenses/MIT)
[![Codecov](https://img.shields.io/codecov/c/github/dedipresta/srules)](https://codecov.io/gh/dedipresta/srules)
![CI](https://github.com/dedipresta/srules/actions/workflows/scala-ci.yml/badge.svg?branch=main)
[![made with love](https://img.shields.io/badge/Made_with-❤-red.svg)](https://www.dedipresta.com)

![Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge-tiny.png)

Rules parsing and evaluation for `Scala 3` built on top of `cats`.

SRules is a library for defining and evaluating rules in Scala 3.
It is inspired by JsonLogic but uses simple strings to define rules instead of JSON.

Motivation:

1. serialization and deserialization of rules
1. access to a context provided by the user (read variables)
1. extensibility with custom operators
1. ability to evaluate to different types (Boolean, Int, Long, Float, Double, String, List)
1. human readable rules

# Table of Contents
1. [Sample Rules](#sample-rules)
1. [Sample rules with SRules Logic](#sample-rules-with-srules-logic)
1. [Installation](#installation)
1. [Usage](#usage)
1. [Understanding Rules Parsing](#understanding-rules-parsing)
    1. [Values of `Expr` and rewriting rules](#values-of-expr-and-rewriting-rules)
    1. [Operators](#operators)
    1. [Variables](#variables)
    1. [Built-in variables](#built-in-variables)
1. [Understanding Rules Evaluation](#understanding-rules-evaluation)
1. [Default Operators](#default-operators)
1. [Building a Custom Operator](#building-a-custom-operator)
   1. [Evaluating Arguments](#evaluating-arguments)
   1. [Syntax Helpers](#syntax-helpers)
1. [Reading the User Context](#reading-the-user-context)
1. [SRules Logic](#srules-logic)
   1. [Simple rules](#simple-rules)
   1. [Combined rules](#combined-rules)
   1. [Logical rule parsing](#logical-rule-parsing)
   1. [Logical rule evaluation](#logical-rule-evaluation)
   1. [Logical rule evaluation report](#logical-rule-evaluation-report)
1. [Show](#show)

## Sample Rules

Aim is to have string based rules that can be parsed and evaluated later with a context.
The result of the evaluation can take different types (numeric, boolean, string, list).

```plaintext
// basic arithmetic
(7 + 3) * 2
```

```plaintext
// access to context variables
($a / 2) * $b - 7
```

```plaintext
// boolean operators
$age > 18 && $country == "FR"
```

```plaintext
// if-(elseif)*-else
if ($x > 0, $value1, $y <= 42, $value2, $default)
```

```plaintext
// functions with variable number of arguments
min($a, $b, $c) < 0
```

```plaintext
// higher order functions
map([1, 2, 3], value() * 2)
```

```plaintext
// acces to current value or index
filter($list, index() % 2 == 0)
```

```plaintext
// access to accumulator
reduce([1, 2, 3], acc() + value())
```

## Sample rules with SRules Logic

Aim is to have JSON based rules with associated names.
Built upon previous rules, these rules will always be evaluated to a boolean and can be composed with combinators (`allOf`, `oneOf`, `noneOf`).

```json
{
   "name": "Free entrance check",
   "allOf": [
      {
         "name": "May be allowed check",
         "oneOf": [
            {
               "name": "Is friend",
               "rule": "contains($friends, $userId)"
            },
            {
               "name": "Has member's card",
               "rule": "hasMemberCard($userId)"
            },
            {
               "name": "Default entry requirements check",
               "rule": "$hasTicket && sayed(\"hello\")"
            }
         ]
      },
      {
         "name": "Excluded entrance check",
         "noneOf": [
            {
               "name": "Blacklist check",
               "rule": "contains($blacklist, $userId)"
            }
         ]
      }
   ]
}
```

## Installation

```plaintext
// ADT + parser + Show
libraryDependencies += "com.dedipresta" %%% "srules-core" % version
// default evaluator and operators
libraryDependencies += "com.dedipresta" %%% "srules-eval" % version
// named rules and logical combinations
libraryDependencies += "com.dedipresta" %%% "srules-logic" % version
// read and write named rules to JSON
libraryDependencies += "com.dedipresta" %%% "srules-logic-circe" % version
```

## Usage

In order to evaluate a rule you will need an effect type with a `MonadError` instance. In the following examples we use Either.

```scala 3
import cats.syntax.all.*
import com.dedipresta.srules.given // for show instance
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.operators.*

// requires a MonadError instance, here we use Either, ExprEvaluatorImpl have instances for Try/ Future
type ErrorOr[A] = Either[EvaluationError, A]

// a given to read variables from your data model, to make it easy we'll use a Map[String, Expr]
// required by the `var` operator, you may use UserContextReader.noContext if you don't need to read variables
type Model = Map[String, Expr]
given UserContextReader[ErrorOr, Model] = UserContextReader.forMapExpr(notFoundToNull = true)
// an evaluator with the operators you want to use
val evaluator: ExprEvaluatorImpl[ErrorOr, Model] = new ExprEvaluatorImpl(DefaultOperators.all)

val model: Model = Map[String, Expr]("var1" -> SRules.parseOrThrow("-42"))

SRules.parse("abs($var1)").flatMap(evaluator.evaluateAll(_, model)) // res: Right(RInt(42))
SRules.parse("abs($var1)").flatMap(evaluator.evaluateAllAs[Int](_, model)) // res: Right(42)
SRules.parse("[1,2,1+1+1]").flatMap(evaluator.evaluateAllAsList[Int](_, model)) // res: Right(List(1, 2, 3))
```

`DefaultOperators.all` is a map that associates the operator name to its implementation allowing (filtering, adding,
aliasing operators).

## Understanding Rules Parsing

Parsing of the rule results in an unevaluated `Expr` ADT.
No context is required to parse a rule .

```scala
import com.dedipresta.srules.*

val rule: Either[cats.parse.Parser.Error, Expr] = SRules.parse("($a + 3) * $b")
```

### Values of `Expr` and rewriting rules

```scala 3
case object RNull extends Expr
case class RBoolean(value: Boolean) extends Expr
case class RInt(value: Int) extends Expr
case class RLong(value: Long) extends Expr
case class RDouble(value: Double) extends Expr
case class RFloat(value: Float) extends Expr
case class RString(value: String) extends Expr
case class RList(value: List[Expr]) extends Expr
case class RFunction(name: String, args: List[Expr]) extends Expr
```

Values are wrapped in their associated `Expr` type.

```scala
// 123.4d is parsed as
RDouble(123.4d)
// [1,2,3] is parsed as
RList(List(RInt(1), RInt(2), RInt(3)))
// null is parsed as
RNull
```

Operators are parsed from the following `generic shape` and rewritten as `RFunction` with the operator name as `name`
and the operands as `args`.

```scala
// someName(arg1, arg2, arg3) is rewritten as
RFunction("someName", List(arg1, arg2, arg3))
```

Here, are some examples of rewriting rules:

```scala
// value() is rewritten as
RFunction("value", List()) // 0 argument
// ceil(7.5d) is rewritten as
RFunction("ceil", List(RDouble(7.5))) // 1 argument
// min(4,5,9,1,8,6) is rewritten as
RFunction("min", List(RInt(4), RInt(5), RInt(9), RInt(1), RInt(8), RInt(6))) // 6 arguments
```

Boolean and arithmetic infix operators have a special treatment to be rewritten as `RFunction`.

```scala
// 7 + 3 is rewritten as
RFunction("+", List(RInt(7), RInt(3)))
```

`Generic shape` is more powerful than the `infix operators` because it can handle any number of arguments.

```scala
// +(1,2,3,4,5) is rewritten as
RFunction("+", List(RInt(1), RInt(2), RInt(3), RInt(4), RInt(5)))
// &&($var1, $var2, $var3)
// ||($var1, $var2, $var3, $var4)
```

#### Variables

Variables are prefixed with `$` or put between `${}` and rewritten as `RFunction` with operator name `var` and the
variable name as the first argument.

A valid variable name starts with a letter or an underscore and is followed by letters, digits or underscores.
Using `${}` allows to use dots separators in the variable name.

```scala
// $a is rewritten as
RFunction("var", List(RString("a")))
```

```scala
// ${a.b.c} is rewritten as
RFunction("var", List(RString("a.b.c")))
```

Using the `generic shape` it becomes possible to define a default value for a variable.

```scala
// var("name", 42) is rewritten as
RFunction("var", List(RString("name"), RInt(42)))
```

Thus, since the name of the variable becomes a simple string, it does not have to follow the parsing rules for variable
names.

```text
var("some->custom->notation")
var("my2DArray[1][4]")
```

##### Built-in variables

There are some built-in variables that can be used in rules:

- `value()` returns the current value in a higher order function (is `${__VALUE__}`)
- `index()` returns the current index in a higher order function (is `${__INDEX__}`)
- `acc()` returns the current accumulator in a higher order function (is `${__ACC__}`)
- `named("abc")` returns the value of a named variable (is `${__NAMED__abc}`)

## Understanding Rules Evaluation

To evaluate a rule, you will need an `ExprEvaluator[F,Ctx,E]` that is configured with your custom rules or uses the
default.
It will evaluate the `Expr` with a given context (your data model) of type `Ctx` and return an `Expr` result or an error
of type `E`.

```scala 3
type ErrorOr[A] = Either[EvaluationError, A]
given UserContextReader[ErrorOr, Map[String, Expr]] = UserContextReader.forMapExpr(notFoundToNull = true)
val evaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)
// evaluate the parsed expression with an empty context
SRules.parse("abs(-42)").flatMap(evaluator.evaluateAll(_, Map.empty))
// res: Right(RInt(42))
```

The evaluator delegates the evaluation to the operators that are provided to it (see `Building a Custom Operator`).

Each operator is responsible for evaluating its arguments and returning the result or an error.

## Default Operators

| Name        | Aliases  | Nb arguments | Description                                                                              | Example                                                                  |
|-------------|----------|--------------|------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| `+`         | `add`    | [0,[         | Addition                                                                                 | `7 + 3`                                                                  |
| `-`         | `sub`    | [1,[         | Subtraction                                                                              | `7 - 3`                                                                  |
| `*`         | `mul`    | [1,[         | Multiplication                                                                           | `7 * 3`                                                                  |
| `/`         | `div`    | 2            | Division                                                                                 | `7 / 3`                                                                  |
| `%`         | `mod`    | 2            | Modulo                                                                                   | `7 % 3`                                                                  |
| `^`         | `pow`    | 2            | Power                                                                                    | `7 ^ 3`                                                                  |
| `==`        | `eq`     | [0,[         | Equality                                                                                 | `$age == 18`                                                             |
| `!=`        | `ne`     | [0,[         | Inequality                                                                               | `$age != 18`                                                             |
| `<`         | `lt`     | [0,[         | Less than                                                                                | `$age < 18`                                                              |
| `<=`        | `le`     | [0,[         | Less than or equal                                                                       | `$age <= 18`                                                             |
| `>`         | `gt`     | [0,[         | Greater than                                                                             | `$age > 18`                                                              |
| `>=`        | `ge`     | [0,[         | Greater than or equal                                                                    | `$age >= 18`                                                             |
| `&&`        | `and`    | [0,[         | Logical and                                                                              | `$age > 18 && $country == "FR"`                                          |
| `\|\|`      | `or`     | [0,[         | Logical or                                                                               | `$age > 18 \|\| $country == "FR"`                                        |
| `!`         | `not`    | 1            | Logical not                                                                              | `!($age > 18)`                                                           |
| `abs`       |          | 1            | Absolute value                                                                           | `abs(-7.5d)`                                                             |
| `acc`       |          | 0            | Access to the accumulator in a higher order function                                     | `reduce([1,2,3], acc()+value())`                                         |
| `atIndex`   |          | 2,3          | Element of a list or string by index, null or default value expr if out of bounds        | `atIndex([1,2,3], 2)`, `atIndex([1,2,3], 4, -1)`, `atIndex("hello", 2)`  |
| `concat`    |          | [1,[         | Concatenation                                                                            | `concat("hello", " world")`, `concat([1,2], [3,4])`                      |
| `contains`  |          | 2            | Check if a list or a string contains a value                                             | `contains([1,2,3], 2)`, `contains("hello", "ll")`                        |
| `exists`    |          | 2            | Check if at least one element of a list satisfies a condition                            | `exists($list, value() > 0)`                                             |
| `if`        |          | 3+2n         | If-elseif-else (returns evaluated result, see `lazyIf`)                                  | `if($x>0, $value1, $y<=42, $value2, $default)`                           |
| `index`     |          | 0            | Access to the current index in a higher order function                                   | `filter($list, index()%2 == 0)`                                          |
| `indexOf`   |          | 2            | Index of an element in a list or string, -1 if not found                                 | `indexOf([1,2,3], 2)`, `indexOf("hello", "ll")`                          |
| `isNull`    |          | 1            | Check if a value is null                                                                 | `isNull($value)`                                                         |
| `ceil`      |          | 1            | Ceiling                                                                                  | `ceil(7.5d)`                                                             |
| `eval`      |          | 1            | Evaluate (to combine with lazy operator, or list of expressions)                         | `eval(($a + 3) * $b)`                                                    |
| `fail`      |          | [0,1]        | Fail with a message                                                                      | `fail("error message")`                                                  |
| `filter`    |          | 2            | Filter                                                                                   | `filter($list, index()%2 == 0)`                                          |
| `find`      |          | 2            | Find an element in a list                                                                | `find($list, value() % 5 == 0)`                                          |
| `floor`     |          | 1            | Floor                                                                                    | `floor(7.5d)`                                                            |
| `forAll`    |          | 2            | Check if all elements of a list satisfy a condition                                      | `forAll($list, value() > 0)`                                             |
| `isEmpty`   |          | 1            | Check if a list or a string is empty                                                     | `isEmpty([1,2,3])`, `isEmpty("hello")`                                   |
| `lazyIf`    |          | 3+2n         | Similar to `if` but does not evaluate returned value                                     | `lazyIf($x>0, $value1, $y<=42, $value2, $default)`                       |
| `map`       |          | 2            | Map                                                                                      | `map([1,2,3], value()*2)`                                                |
| `max`       |          | [1,[         | Maximum                                                                                  | `max(4,5,9,1,8,6)`                                                       |
| `min`       |          | [1,[         | Minimum                                                                                  | `min(4,5,9,1,8,6)`                                                       |
| `named`     |          | 1,3          | Named value ; named(name) to read it ; named(name, value, exprWithNamed) to evaluate ;   | `named("i", $var1, if(isNull(named("i")), -1, named("i")*2))`            |
| `nonEmpty`  |          | 1            | Check if a list or a string is not empty                                                 | `nonEmpty([1,2,3])`, `nonEmpty("hello")`                                 |
| `notNull`   |          | 1            | Check if a value is not null                                                             | `notNull($value)`                                                        |
| `reduce`    |          | 2            | Reduce                                                                                   | `reduce([1,2,3], acc()+value())`                                         |
| `round`     |          | 1            | Round                                                                                    | `round(7.5d)`                                                            |
| `size`      |          | 1            | Get the size of a list or a string                                                       | `size([1,2,3])`, `size("hello")`                                         |
| `toInt`     | `int`    | 1            | Convert to Int                                                                           | `toInt(7.5d)`                                                            |
| `toLong`    | `long`   | 1            | Convert to Long                                                                          | `toLong(7.5d)`                                                           |
| `toFloat`   | `float`  | 1            | Convert to Float                                                                         | `toFloat(7.5d)`                                                          |
| `toDouble`  | `double` | 1            | Convert to Double                                                                        | `toDouble(7.5d)`                                                         |
| `toString`  | `string` | 1            | Convert to String                                                                        | `toString(7.5d)`                                                         |
| `toBoolean` | `bool`   | 1            | Convert to Boolean                                                                       | `toBoolean(7.5d)`                                                        |
| `var`       |          | [1,2]        | Access to context variables `You should implement UserContextReader[Ctx] for your model` | `$age > 18`, `var("name", 42)`                                           |
| `value`     |          | 0            | Access to the current value in a higher order function                                   | `map([1,2,3], value()*2)`                                                |

## Building a Custom Operator

You may want to add your own operators or replace existing ones that do not fit your needs.

You'll need to implement `Operator` trait and more precisely its `evaluate` method.

```scala 3
trait Operator[F[_], Ctx, E]:
  def evaluate(
                evaluator: ExprEvaluator[F, Ctx, E], // reference to the evaluator so that we can evaluate sub-expressions
                op: String, // current operator name mostly provided for error messages
                args: List[Expr], // arguments of the operator
                ctx: RuleCtx[Ctx], // context of the evaluation (user context, named variables, current index or value)
              ): F[Expr] // result of the evaluation or an error
```

1. If your operator wants an access to the user provided context, you should require a `UserContextReader[Ctx]` (see the
   dedicated part of the docs).
1. Then you need to evaluate the arguments you will use in your operator (see `Evaluating Arguments`).
1. Finally, you can provide your custom logic and return the result as an `Expr` or an error message in a `F[_]`.

Let's take an example with `isEmpty` that checks if a string or a list is empty.

```scala 3
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object IsEmpty:

  def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
    new Operator[F, Ctx, EvaluationError]:
      def evaluate(
                    evaluator: ExprEvaluator[F, Ctx, EvaluationError],
                    op: String,
                    args: List[Expr],
                    ctx: RuleCtx[Ctx],
                  ): F[Expr] =
        args // raw args of the operator (may reference variables, or other operators)
          .traverse(evaluator.deepEvaluateFunctions(_, ctx)) // evaluation of the arguments before use
          .flatMap(_.withExactly1[F](op)) // some helpers that check that the number of arguments is correct
          .flatMap {
            case Expr.RString(s) => s.isEmpty.toExpr.pure[F] // check emptiness, transform to Expr and lift to F
            case Expr.RList(l) => l.isEmpty.toExpr.pure[F]
            case other => EvaluationError.OperationFailure(op, args, FailureReason.InvalidArgumentType("String or List", other)).raiseError[F, Expr]
          }
```

Now, with an operator that evaluates the arguments one by one. The boolean operator `OR`, `||`.

```scala 3
import com.dedipresta.srules.*
import com.dedipresta.srules.evaluate.*
import com.dedipresta.srules.evaluate.syntax.*

import cats.MonadError
import cats.syntax.all.*

object Or:

def apply[F[_], Ctx]()(using F: MonadError[F, EvaluationError]): Operator[F, Ctx, EvaluationError] =
  new Operator[F, Ctx, EvaluationError]:
    def evaluate(
                  evaluator: ExprEvaluator[F, Ctx, EvaluationError],
                  op: String,
                  args: List[Expr],
                  ctx: RuleCtx[Ctx],
                ): F[Expr] =
      // no traverse since we want to short-circuit evaluation as soon as we find a true value
      args
        .foldLeft(false.pure[F])((acc, v) => // neutral element for OR is false
          acc.flatMap {
            case true => acc // bool condition is true, no need to evaluate the rest of the arguments
            // evaluate before use ; it becomes the next acc value ; false || other = other)
            case _ => evaluator.evaluatedToBoolean(op, v, ctx)
          },
        )
        .map(_.toExpr)
```

### Evaluating Arguments

When evaluating the arguments of an operator, you may want to evaluate them all at once (traverse) or one by one (e.g
foldLeft) if you want to short-circuit the evaluation.

1. generally you will need to greedily evaluate an argument so you may use the syntax helper
   `evaluator.deepEvaluateFunctions` that will evaluate `the expression and its result` when it is a function. It allows
   to handle operators that return complex expressions (functions).
1. when you do not want to evaluate the result of a function, you may use `evaluator.evaluate` directly.
1. when you want a deep evaluation of functions and list of expressions, you may use
   `evaluator.deepEvaluateFunctionsAndLists`.

Let's take an example to understand the difference between `evaluate`, `deepEvaluateFunctions` and
`deepEvaluateFunctionsAndLists`.

Here, the operator that will return complex expressions (functions) will be our `var` operator that reads from a context
that may return `Expr` being complex expressions.

```scala
val userCtx: Map[String, Expr] = Map[String, Expr](
  "simpleFunction" -> SRules.parseOrThrow("1+1"), // RFunction("+", List(RInt(1), RInt(1)))
  "unevaluatedList" -> SRules.parseOrThrow("[1, $simpleFunction]"), // RList(List(RInt(1), RFunction("var", List(RString("simpleFunction")))))
)

// parsing and evaluation of expr=`$simpleFunction` then, call of `show` on the result
// evaluator.evaluate                       """(1+1)""" // var returns a function, evaluate returns it as is
// evaluator.deepEvaluateFunctions          """2""" // var return a function, deepEvaluateFunctions evaluates it
// evaluator.deepEvaluateFunctionsAndLists  """2""" // var returns a function, deepEvaluateFunctionsAndLists evaluates it

// parsing and evaluation of expr=`$unevaluatedList` then, call of `show` on the result
// evaluator.evaluate                       """[1,var("simpleFunction")]""" // var returns a list, evaluate returns it as is
// evaluator.deepEvaluateFunctions          """[1,var("simpleFunction")]""" // var returns a list, deepEvaluateFunctions returns it as is
// evaluator.deepEvaluateFunctionsAndLists  """[1,2]""" // var returns a list, deepEvaluateFunctionsAndLists evaluates its expressions
```

### Syntax Helpers

When writing an operator, you may use some syntax helpers to extract values from the arguments or check the count of
them, just import the syntax helpers.

```scala
import com.dedipresta.srules.evaluate.syntax.*
```

## Reading the User Context

Most of the time, operators will not need to access the user context, but will only pass it to evaluate sub-expressions.

To allow an operator to access the user context you should give it a `UserContextReader[Ctx]` as a given parameter, then
you may use this instance to read variables. `Ctx` is the type of your data model.
Only the `var` operator is accessing the user context by default.

```scala 3
trait UserContextReader[F[_], Ctx]:
  def read(ctx: Ctx, name: String, defaultValue: Option[Expr]): F[Expr]
```

It means that you have to provide a way to read a variable, by name, from your data model and return an `Expr` so it can
be used in the evaluation.

It also means that you can define how to handle requested variables that are not found in your data model, like
returning `Expr.RNull` or an error.

As an example, let's consider an implementation for a context that is a `Map[String, Expr]` (available with
`UserContextReader.forMapExpr`).

```scala 3
def forMapExpr[F[_]](notFoundToNull: Boolean)(using F: MonadError[F, EvaluationError]): UserContextReader[F, Map[String, Expr]] =
  new UserContextReader[F, Map[String, Expr]]:
    def read(ctx: Map[String, Expr], name: String, defaultValue: Option[Expr]): F[Expr] =
      ctx.get(name) match {
        case Some(value) => value.pure[F]
        case None => handleNotFound(name, defaultValue, notFoundToNull)
      }

    private def handleNotFound[F[_]](name: String, defaultValue: Option[Expr], notFoundToNull: Boolean)(using
                                                                                                        F: MonadError[F, EvaluationError],
    ): F[Expr] =
      defaultValue match
        case Some(v) => v.pure[F]
        case None => if (notFoundToNull) Expr.RNull.pure[F] else F.raiseError(FailureReason.VariableNotFound(name).opError("readUserContext", Nil))
```

`WARNING` you may not directly use a `Map[String, Any]` when using the `scala.js` runtime since pattern matching on
numeric types from `Any` is not reliable.
For example 42.0f will be seen as an Int and there is no way to distinguish between Float and Double, ...
So you'll have to wrap the values in a custom type (here we use `Expr`) or provide some type information in the data or
in the var name, ...

## SRules Logic

`srules-logic` and its associated artifact `srules-logic-circe` can be used to read or write json strings/ files containing named rules that evaluate to boolean values.

There are 2 kinds of rules:
1. simple rules: that associate a name to a rule
2. combined rules: that associate a name with a combinator and a non empty list of rules


### Simple rules

```json
{
   "name": "My simple name",
   "rule": "$myVar > 0"
}
```

### Combined rules

Allowed combinators are `allOf`, `oneOf`, `noneOf`.

```json
{
  "name": "My combined rules",
  "allOf": [
    {
      "name": "First rule",
      "rule": "$myVar > 0"
    },
    {
      "name": "Other combined rules",
      "oneOf": [
        {
          "name": "Is over 9000",
          "rule": "$myVar > 9000"
        },
        {
          "name": "Is 42",
          "rule": "$myVar == 42"
        }
      ]
    }
  ]
}
```

### Logical rule parsing

```scala 3
import com.dedipresta.srules.*
import com.dedipresta.srules.logic.LogicalRule
import com.dedipresta.srules.logic.LogicalRule.*
import com.dedipresta.srules.logic.circe.given // encoders and decoders

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

val s =
   """{
     |  "name": "Some name",
     |  "rule": "$a > 0"
     |}
     |""".stripMargin

parse(s).flatMap(_.as[LogicalRule]) == Right(SimpleRule("Some name", SRules.parseOrThrow("$a > 0"))),
// true
```

### Logical rule evaluation

Evaluating a logical rule is similar to what you may do for other rules.

```scala 3
type ErrorOr[A] = Either[EvaluationError, A]
given UserContextReader[ErrorOr, Map[String, Expr]]              = UserContextReader.forMapExpr(notFoundToNull = true)
val exprEvaluator: ExprEvaluatorImpl[ErrorOr, Map[String, Expr]] = new ExprEvaluatorImpl(DefaultOperators.all)

// specific to logical rules
val evaluator = new LogicalEvaluatorImpl[ErrorOr, Map[String, Expr]](exprEvaluator)

val rule = SimpleRule("some rule name", true.toExpr)
evaluator.evaluate(rule, Map.empty) // Right(true)
```

### Logical rule evaluation report

You may also use `evaluator.evaluateWithReport` to get a `Report` to get the details of the evaluation instead of the final result.

```scala 3
final case class Report(
    name: String,
    satisfied: Boolean,
    combinator: Option[LogicalCombinator],
    details: List[Report],
)
```

`Report` has `circe` encoder and decoder.

## Show

```scala 3
import com.dedipresta.srules.given // for show instance
```

A `Show[Expr]` instance is provided to serialize an `Expr` to a human readable string.
The resulting string is a valid rule that can be parsed again, to the same `Expr`.
Nonetheless, it may differ from the original String (extra parentheses, explicit types, ...).

