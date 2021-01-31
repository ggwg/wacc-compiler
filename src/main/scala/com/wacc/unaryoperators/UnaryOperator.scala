package com.wacc.unaryoperators

trait UnaryOperator {}

object UnaryOperator {
  def apply(operator: String): UnaryOperator = operator match {
    case "!"   => new Not()
    case "-"   => new Negate()
    case "len" => new Length()
    case "ord" => new Ord()
    case "chr" => new Chr()
  }
}
