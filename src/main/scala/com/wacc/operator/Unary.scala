package com.wacc.operator

sealed trait UnaryOperator

case class Chr() extends UnaryOperator {
  override def toString: String = "chr"
}
case class Length() extends UnaryOperator {
  override def toString: String = "len"
}
case class Negate() extends UnaryOperator {
  override def toString: String = "-"
}
case class Not() extends UnaryOperator {
  override def toString: String = "!"
}
case class Ord() extends UnaryOperator {
  override def toString: String = "ord"
}

object UnaryOperator {
  val build: String => UnaryOperator = UnaryOperator(_)
  val operators = List("!", "-", "len", "ord", "chr")
  def apply(operator: String): UnaryOperator = operator match {
    case "!"   => Not()
    case "-"   => Negate()
    case "len" => Length()
    case "ord" => Ord()
    case "chr" => Chr()
  }
}
