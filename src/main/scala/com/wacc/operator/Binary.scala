package com.wacc.operator

trait BinaryOperator {}

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

object BinaryOperator {
  val build: String => BinaryOperator = BinaryOperator(_)
  val operators =
    List("*", "/", "%", "+", "-", ">", ">+", "<", "<=", "==", "!=", "&&", "||")
  def apply(operator: String): BinaryOperator = operator match {
    case "*"  => Multiply()
    case "/"  => Divide()
    case "%"  => Modulo()
    case "+"  => Add()
    case "-"  => Subtract()
    case ">"  => GreaterThan()
    case ">=" => GreaterEqualThan()
    case "<"  => SmallerThan()
    case "<=" => SmallerEqualThan()
    case "==" => Equals()
    case "!=" => NotEquals()
    case "&&" => And()
    case "||" => Or()
  }
}
