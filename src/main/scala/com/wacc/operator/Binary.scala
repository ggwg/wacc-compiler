package com.wacc.operator

import com.wacc
import com.wacc.{ASTNodeVoid, SymbolTable}
import parsley.Parsley

sealed trait BinaryOperator extends ASTNodeVoid

case class Add() extends BinaryOperator {
  override def toString: String = "+"
}
case class And() extends BinaryOperator {
  override def toString: String = "&&"
}
case class Divide() extends BinaryOperator {
  override def toString: String = "/"
}
case class Equals() extends BinaryOperator {
  override def toString: String = "=="
}
case class GreaterEqualThan() extends BinaryOperator {
  override def toString: String = ">="
}
case class GreaterThan() extends BinaryOperator {
  override def toString: String = ">"
}
case class Modulo() extends BinaryOperator {
  override def toString: String = "%"
}
case class Multiply() extends BinaryOperator {
  override def toString: String = "*"
}
case class NotEquals() extends BinaryOperator {
  override def toString: String = "!="
}
case class Or() extends BinaryOperator {
  override def toString: String = "||"
}
case class SmallerEqualThan() extends BinaryOperator {
  override def toString: String = "<="
}
case class SmallerThan() extends BinaryOperator {
  override def toString: String = "<"
}
case class Subtract() extends BinaryOperator {
  override def toString: String = "-"
}

object BinaryOperator {
  val operators =
    List(">=", "<=", "==", "!=", "&&", "||", "*", "/", "%", "+", "-", ">", "<")
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

  def apply(operator: Parsley[String]): Parsley[BinaryOperator] =
    operator.map(BinaryOperator(_))
}
