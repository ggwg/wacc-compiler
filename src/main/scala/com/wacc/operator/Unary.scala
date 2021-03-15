package com.wacc.operator

import com.wacc
import com.wacc.{ASTNodeVoid, SymbolTable}
import parsley.Parsley

sealed trait UnaryOperator extends ASTNodeVoid

case class Chr() extends UnaryOperator {
  override def toString: String = "chr "
}
case class Length() extends UnaryOperator {
  override def toString: String = "len "
}
case class Negate() extends UnaryOperator {
  override def toString: String = "-"
}
case class Not() extends UnaryOperator {
  override def toString: String = "!"
}
case class Ord() extends UnaryOperator {
  override def toString: String = "ord "
}
case class PrefixInc() extends UnaryOperator {
  override def toString: String = "++"
}
case class PrefixDec() extends UnaryOperator {
  override def toString: String = "--"
}

object UnaryOperator {
  val operators = List("!", "-", "len", "ord", "chr", "++", "--")

  def apply(operator: String): UnaryOperator = operator match {
    case "!"   => Not()
    case "-"   => Negate()
    case "len" => Length()
    case "ord" => Ord()
    case "chr" => Chr()
    case "++"  => PrefixInc()
    case "--"  => PrefixDec()
  }

  def apply(operator: Parsley[String]): Parsley[UnaryOperator] =
    operator.map(UnaryOperator(_))
}
