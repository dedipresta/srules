package com.dedipresta.srules.logic.circe

import com.dedipresta.srules.*
import com.dedipresta.srules.given_Show_Expr
import com.dedipresta.srules.logic
import com.dedipresta.srules.logic.*
import com.dedipresta.srules.logic.LogicalRule.*

import cats.*
import cats.data.*
import cats.syntax.all.*
import io.circe.*

extension (l: LogicalCombinator.type) {

  inline def AllOfString: String  = "allOf"
  inline def OneOfString: String  = "oneOf"
  inline def NoneOfString: String = "noneOf"

  def fromString(s: String): Either[String, LogicalCombinator] =
    s match {
      case "allOf"  => LogicalCombinator.AllOf.asRight
      case "oneOf"  => LogicalCombinator.OneOf.asRight
      case "noneOf" => LogicalCombinator.NoneOf.asRight
      case other    => s"value '$other' is not a supported combinator, allowed values are ['$AllOfString', '$OneOfString', '$NoneOfString']".asLeft
    }

  def stringValue(c: LogicalCombinator): String =
    c match {
      case LogicalCombinator.AllOf  => AllOfString
      case LogicalCombinator.OneOf  => OneOfString
      case LogicalCombinator.NoneOf => NoneOfString
    }

}

given Codec[LogicalCombinator] = {

  val encoder: Encoder[LogicalCombinator] =
    Encoder.encodeString.contramap(LogicalCombinator.stringValue)

  val decoder: Decoder[LogicalCombinator] =
    Decoder.decodeString.emap(LogicalCombinator.fromString)

  Codec.from(decoder, encoder)
}

given Decoder[LogicalRule] = {

  val simpleRuleDecoder: Decoder[SimpleRule] =
    new Decoder[SimpleRule] {
      final def apply(c: HCursor): Decoder.Result[SimpleRule] =
        for {
          rule <-
            c.downField("rule")
              .as[String]
              .flatMap(s => SRules.parse(s).leftMap(e => DecodingFailure(DecodingFailure.Reason.CustomReason(s"rule cannot be parsed: ${e.show}"), c)))
          name <- c.downField("name").as[String]
        } yield SimpleRule(name, rule)
    }

  def combinedRulesDecoder(enc: Decoder[LogicalRule]): Decoder[CombinedRules] =
    new Decoder[CombinedRules] {
      final def apply(c: HCursor): Decoder.Result[CombinedRules] =
        for {
          name               <- c.downField("name").as[String]
          combinatorAndRules <-
            c.downField("allOf")
              .as[NonEmptyList[LogicalRule]]
              .map((LogicalCombinator.AllOf, _))
              .orElse(c.downField("oneOf").as[NonEmptyList[LogicalRule]].map((LogicalCombinator.OneOf, _)))
              .orElse(c.downField("noneOf").as[NonEmptyList[LogicalRule]].map((LogicalCombinator.NoneOf, _)))
        } yield CombinedRules(name, combinatorAndRules._1, combinatorAndRules._2)
    }

  lazy val dec: Decoder[LogicalRule] =
    simpleRuleDecoder
      .widen[LogicalRule]
      .or(combinedRulesDecoder(dec).widen[LogicalRule])

  dec

}

given Encoder[LogicalRule] =
  new Encoder[LogicalRule]:
    final def apply(a: LogicalRule): Json =
      a match {
        case r: SimpleRule    =>
          Json.obj(
            "name" -> Json.fromString(r.name),
            "rule" -> Json.fromString(r.rule.show),
          )
        case r: CombinedRules =>
          Json.obj(
            "name"                                      -> Json.fromString(r.name),
            LogicalCombinator.stringValue(r.combinator) -> Json.arr(r.rules.toList.map(this.apply)*),
          )
      }

given Encoder[Report] =
  new Encoder[Report] {
    final def apply(a: Report): Json =
      Json.obj(
        "name"       -> Json.fromString(a.name),
        "satisfied"  -> Json.fromBoolean(a.satisfied),
        "combinator" -> Json.fromStringOrNull(a.combinator.map(LogicalCombinator.stringValue)),
        "details"    -> Json.arr(a.details.map(apply)*),
      )
  }

given Decoder[Report] = {
  lazy val decoder: Decoder[Report] =
    new Decoder[Report]:
      final def apply(c: HCursor): Decoder.Result[Report] =
        for {
          name       <- c.downField("name").as[String]
          satisfied  <- c.downField("satisfied").as[Boolean]
          combinator <- c.downField("combinator").as[Option[LogicalCombinator]]
          details    <- c.downField("details").as[List[Report]]
        } yield Report(name, satisfied, combinator, details)

  decoder
}
