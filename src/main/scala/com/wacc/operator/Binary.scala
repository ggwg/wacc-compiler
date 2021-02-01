package com.wacc.operator

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

case class Add() extends BinaryOperator
case class And() extends BinaryOperator
case class Divide() extends BinaryOperator
case class Equals() extends BinaryOperator
case class GreaterEqualThan() extends BinaryOperator
case class GreaterThan() extends BinaryOperator
case class Modulo() extends BinaryOperator
case class Multiply() extends BinaryOperator
case class NotEquals() extends BinaryOperator
case class Or() extends BinaryOperator
case class SmallerEqualThan() extends BinaryOperator
case class SmallerThan() extends BinaryOperator
case class Subtract() extends BinaryOperator
