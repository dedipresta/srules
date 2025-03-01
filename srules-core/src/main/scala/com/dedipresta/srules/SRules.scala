package com.dedipresta.srules

import cats.parse.Parser as CatsParser

object SRules:

  def parse(s: String): Either[CatsParser.Error, Expr] =
    Parser.parser.parseAll(s)

  def parseOrThrow(s: String): Expr =
    parse(s).fold(e => throw new Exception(e.toString), identity)
