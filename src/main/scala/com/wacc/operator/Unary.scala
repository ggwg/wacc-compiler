package com.wacc.operator

sealed trait UnaryOperator {}

object UnaryOperator {
  def apply(operator: String): UnaryOperator = operator match {
    case "!"   => Not()
    case "-"   => Negate()
    case "len" => Length()
    case "ord" => Ord()
    case "chr" => Chr()
  }
}

case class Chr() extends UnaryOperator
case class Length() extends UnaryOperator
case class Negate() extends UnaryOperator
case class Not() extends UnaryOperator
case class Ord() extends UnaryOperator
