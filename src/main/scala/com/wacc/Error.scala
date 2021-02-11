package com.wacc

trait Error {}

case class DefaultError(message: String, pos: (Int, Int)) extends Error {
  def throwError(): Unit =
    Console.out.println(
      Console.RED + "(Error Code 200) " +
        Console.RESET + "Semantic error on line " + pos._1 + ", column " + pos._2 + ":\n\t- " + message
    )
}

case class UnaryOperatorError(op: String, expected: String, actual: String, pos: (Int, Int)) extends Error {
  def throwError(): Unit =
    DefaultError("Pre-defined function " + op + " expected " + expected + ", but found " + actual, pos).throwError()
}

case class BinaryOperatorError(op: String, expected: String, actual: String, pos: (Int, Int), isLeft: Boolean)
    extends Error {
  def throwError(): Unit = {
    val side = if (isLeft) "left" else "right"
    DefaultError(
      "Pre-defined function " + op + " expected " + expected + " for " + side + " operand, but found " + actual,
      pos
    )
      .throwError()
  }
}
