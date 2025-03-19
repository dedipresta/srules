package com.dedipresta.srules.logic.circe

import com.dedipresta.srules.*
import com.dedipresta.srules.logic.*
import com.dedipresta.srules.logic.LogicalCombinator.*
import com.dedipresta.srules.logic.LogicalRule.*
import com.dedipresta.srules.logic.circe.given

import cats.data.NonEmptyList
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import munit.*

final class CirceSuite extends FunSuite {

  test("decoder should be able to decode a logical rule (simple rule)") {
    val s =
      """{
        |  "name": "Some name",
        |  "rule": "$a > 0"
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]),
      Right(SimpleRule("Some name", SRules.parseOrThrow("$a > 0"))),
    )
  }

  test("decoder should be able to decode a combined rules (allOf)") {
    val s =
      """{
        |  "name": "Some name",
        |  "allOf": [
        |    {
        |      "name": "Sub rule",
        |      "rule": "$a > 0"
        |    }
        |  ]
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]),
      Right(CombinedRules("Some name", AllOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))),
    )
  }

  test("decoder should be able to decode a combined rules (oneOf)") {
    val s =
      """{
        |  "name": "Some name",
        |  "oneOf": [
        |    {
        |      "name": "Sub rule",
        |      "rule": "$a > 0"
        |    }
        |  ]
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]),
      Right(CombinedRules("Some name", OneOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))),
    )
  }

  test("decoder should be able to decode a combined rules (noneOf)") {
    val s =
      """{
        |  "name": "Some name",
        |  "noneOf": [
        |    {
        |      "name": "Sub rule",
        |      "rule": "$a > 0"
        |    }
        |  ]
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]),
      Right(CombinedRules("Some name", NoneOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))),
    )
  }

  test("decoder should be able to decode nested combined rules") {
    val s =
      """{
        |  "name": "Some name 1",
        |  "allOf": [
        |    {
        |      "name": "Sub rule 1",
        |      "rule": "$a > 1"
        |    },
        |    {
        |      "name": "Sub rule 2",
        |      "rule": "$b > 2"
        |    },
        |    {
        |      "name": "Sub rule 3",
        |      "oneOf": [
        |        {
        |          "name": "Sub sub rule 1",
        |          "rule": "$c > 3"
        |        }
        |      ]
        |    }
        |  ]
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]),
      Right(
        CombinedRules(
          "Some name 1",
          AllOf,
          NonEmptyList.of(
            SimpleRule("Sub rule 1", SRules.parseOrThrow("$a > 1")),
            SimpleRule("Sub rule 2", SRules.parseOrThrow("$b > 2")),
            CombinedRules(
              "Sub rule 3",
              OneOf,
              NonEmptyList.one(
                SimpleRule("Sub sub rule 1", SRules.parseOrThrow("$c > 3")),
              ),
            ),
          ),
        ),
      ),
    )
  }

  test("can decode after encoding (simple rule") {
    val rule: LogicalRule = SimpleRule("Some name", SRules.parseOrThrow("$a > 0"))
    assertEquals(parse(rule.asJson.noSpaces).flatMap(_.as[LogicalRule]), Right(rule))
  }

  test("can decode after encoding (combined rules allOf)") {
    val rule: LogicalRule = CombinedRules("Some name", AllOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))
    assertEquals(parse(rule.asJson.noSpaces).flatMap(_.as[LogicalRule]), Right(rule))
  }

  test("can decode after encoding (combined rules oneOf)") {
    val rule: LogicalRule = CombinedRules("Some name", OneOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))
    assertEquals(parse(rule.asJson.noSpaces).flatMap(_.as[LogicalRule]), Right(rule))
  }

  test("can decode after encoding (combined rules noneOf)") {
    val rule: LogicalRule = CombinedRules("Some name", NoneOf, NonEmptyList.one(SimpleRule("Sub rule", SRules.parseOrThrow("$a > 0"))))
    assertEquals(parse(rule.asJson.noSpaces).flatMap(_.as[LogicalRule]), Right(rule))
  }

  test("raise a decoding failure when rule canno be parsed") {
    val s =
      """{
        |  "name": "Some name",
        |  "rule": ")$a > 0"
        |}
        |""".stripMargin

    assertEquals(
      parse(s).flatMap(_.as[LogicalRule]).swap.toOption.map(_.isInstanceOf[DecodingFailure]),
      Some(true),
    )
  }

  test("allow to read and write a report") {

    val report =
      Report(
        "rule allOf",
        true,
        Some(LogicalCombinator.AllOf),
        List(
          Report(
            "rule oneOf",
            true,
            Some(LogicalCombinator.OneOf),
            List(
              Report(
                "simple rule 1",
                true,
                None,
                Nil,
              ),
            ),
          ),
          Report(
            "rule noneOf",
            true,
            Some(LogicalCombinator.NoneOf),
            List(
              Report(
                "simple rule 2",
                false,
                None,
                Nil,
              ),
            ),
          ),
        ),
      )

    assertEquals(
      parse(report.asJson.noSpaces).flatMap(_.as[Report]),
      Right(report),
    )

    test("has a decoder for logical combinator (allOf)") {
      assertEquals(
        parse("allOf").flatMap(_.as[LogicalCombinator]),
        Right(LogicalCombinator.AllOf),
      )
    }

    test("has a decoder for logical combinator (oneOf)") {
      assertEquals(
        parse("oneOf").flatMap(_.as[LogicalCombinator]),
        Right(LogicalCombinator.OneOf),
      )
    }

    test("has a decoder for logical combinator (noneOf)") {
      assertEquals(
        parse("noneOf").flatMap(_.as[LogicalCombinator]),
        Right(LogicalCombinator.NoneOf),
      )
    }

    test("has a decoder for logical combinator (fail for unknown)") {
      assertEquals(
        parse("other").flatMap(_.as[LogicalCombinator]).isLeft,
        true,
      )
      assertEquals(
        parse("other").flatMap(_.as[LogicalCombinator]).swap.toString,
        "value 'other' is not a supported combinator, allowed values are [allOf, oneOf, noneOf]",
      )
      assertEquals(
        LogicalCombinator.fromString("other"),
        Left("value 'other' is not a supported combinator, allowed values are [allOf, oneOf, noneOf]"),
      )
    }

    test("has an encoder for logical combinator (allOf)") {
      val combinator: LogicalCombinator = LogicalCombinator.AllOf
      assertEquals(
        combinator.asJson.noSpaces,
        "allOf",
      )
    }

    test("has an encoder for logical combinator (oneOf)") {
      val combinator: LogicalCombinator = LogicalCombinator.OneOf
      assertEquals(
        combinator.asJson.noSpaces,
        "oneOf",
      )
    }

    test("has an encoder for logical combinator (noneOf)") {
      val combinator: LogicalCombinator = LogicalCombinator.NoneOf
      assertEquals(
        combinator.asJson.noSpaces,
        "noneOf",
      )
    }

    test("report decoding should fail when combinator is not recognized") {
      val s =
        """{
          |  "name": "Some name",
          |  "unknown": [
          |    {
          |      "name": "Sub rule",
          |      "rule": "$a > 0"
          |    }
          |  ]
          |}
          |""".stripMargin

      assertEquals(
        parse(s).flatMap(_.as[LogicalRule]).swap.toOption.map(_.isInstanceOf[DecodingFailure]),
        Some(true),
      )
    }

  }

}
