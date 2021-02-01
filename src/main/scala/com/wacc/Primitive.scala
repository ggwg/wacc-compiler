package com.wacc

case class DefaultCharacter(char: Char, isEscaped: Boolean) {
  override def toString: String = isEscaped match {
    case true  => "\\" + char
    case false => "" + char
  }
}

case class Digit(digit: Char) {
  override def toString: String = digit.toString
}

case class EscapedCharacter(char: Char) {
  override def toString: String = char.toString
}

case class IntegerSign(sign: Char) {
  override def toString: String = sign.toString
}

object DefaultCharacter {
  val build: (Char, Boolean) => DefaultCharacter = DefaultCharacter(_, _)
}

object Digit {
  val build: (Char) => Digit = Digit(_)
}

object EscapedCharacter {
  val escapableCharacters = "0btnfr\"\'\\"

  val build: (Char => EscapedCharacter) = EscapedCharacter(_)
}

object IntegerSign {
  val build: Char => IntegerSign = IntegerSign(_)
}
