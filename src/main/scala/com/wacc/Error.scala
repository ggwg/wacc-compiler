package com.wacc

trait Error {
  def getError(): Error
  def throwError(): Unit = ()
}

case class DefaultError(message: String, pos: (Int, Int)) extends Error {
  override def throwError(): Unit = {
    Console.out.println(
      Console.RED + "(Error Code 200) " +
        Console.RESET + "Semantic error on line " + pos._1 + ", column " + pos._2 + ":\n\t- " + message
    )
  }
  override def getError(): Error = this
}

case class UnaryOperatorError(op: String, expected: String, actual: String, pos: (Int, Int)) extends Error {
  def getError(): Error =
    DefaultError("Pre-defined function " + op + " expected " + expected + ", but found " + actual, pos)
}

case class BinaryOperatorError(op: String, expected: String, actual: String, pos: (Int, Int), isLeft: Boolean)
    extends Error {
  def getError(): Error = {
    val side = if (isLeft) "left" else "right"
    DefaultError(
      "Pre-defined function " + op + " expected " + expected + " for " + side + " operand, but found " + actual,
      pos
    )
  }
}
