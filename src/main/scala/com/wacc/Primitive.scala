package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

case class DefaultCharacter(char: Char, isEscaped: Boolean) extends ASTNodeVoid {
  override def toString: String = if (isEscaped) {
    "\\" + char
  } else {
    "" + char
  }
}

case class Digit(digit: Char) extends ASTNodeVoid {
  override def toString: String = digit.toString
}

case class EscapedCharacter(char: Char) extends ASTNodeVoid {
  override def toString: String = char.toString
}

case class IntegerSign(sign: Char) extends ASTNodeVoid {
  override def toString: String = sign.toString
}

object DefaultCharacter {
  def apply(chr: Parsley[Char], isEscaped: Boolean): Parsley[DefaultCharacter] =
    chr.map(DefaultCharacter(_, isEscaped))
}

object Digit {
  val digits = "0123456789"
  def apply(chr: Parsley[Char]): Parsley[Digit] = chr.map(Digit(_))
}

object EscapedCharacter {
  val escapableCharacters = "0btnfr\"\'\\"
  def apply(chr: Parsley[Char]): Parsley[EscapedCharacter] = chr.map(EscapedCharacter(_))
}

object IntegerSign {
  def apply(chr: Parsley[Char]): Parsley[IntegerSign] = chr.map(IntegerSign(_))
}
