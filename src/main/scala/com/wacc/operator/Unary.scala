package com.wacc.operator

sealed trait UnaryOperator {}

object UnaryOperator {
  def apply(operator: String): UnaryOperator = operator match {
    case "!"   => new Not()
    case "-"   => new Negate()
    case "len" => new Length()
    case "ord" => new Ord()
    case "chr" => new Chr()
  }
}

case class Chr() extends UnaryOperator
case class Length() extends UnaryOperator
case class Negate() extends UnaryOperator
case class Not() extends UnaryOperator
case class Ord() extends UnaryOperator
