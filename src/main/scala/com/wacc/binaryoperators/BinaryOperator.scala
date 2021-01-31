package com.wacc.binaryoperators

trait BinaryOperator {}

object BinaryOperator {
  def apply(operator: String): BinaryOperator = operator match {
    case "*"  => new Multiply()
    case "/"  => new Divide()
    case "%"  => new Modulo()
    case "+"  => new Add()
    case "-"  => new Subtract()
    case ">"  => new GreaterThan()
    case ">=" => new GreaterEqualThan()
    case "<"  => new SmallerThan()
    case "<=" => new SmallerEqualThan()
    case "==" => new Equals()
    case "!=" => new NotEquals()
    case "&&" => new And()
    case "||" => new Or()
  }
}
