package com.wacc

case class DefaultCharacter(char: Char, isEscaped: Boolean)

case class Digit(digit: Char)

case class EscapedCharacter(char: Char)

case class IntegerSign(sign: Char)

object EscapedCharacter {
  val escapableCharacters = "0btnfr\"\'\\"
}
