package com.wacc

case class DefaultCharacter(val char: Char, val isEscaped: Boolean)

case class Digit(val digit: Char)

case class EscapedCharacter(val char: Char)

case class IntegerSign(val sign: Char)

object EscapedCharacter {
  val escapableCharacters = "0btnfr\"\'\\"
}
