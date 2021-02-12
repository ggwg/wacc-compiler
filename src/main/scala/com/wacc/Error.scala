package com.wacc

case class Error(message: String, position: (Int, Int), code: Int = 200) {
  def throwError(): Unit = {
    Console.out.println(
      Error.formatRed(
        "(Error Code" + code + ") "
      ) + "Semantic error on line " + position._1 + ", column " + position._2 + ":\n\t- " + message
    )
  }
}

case object UnaryOperatorError {
  def expectation(operator: String, expected: String, actual: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Pre-defined function " + operator + " expected " + Error.formatYellow(expected) +
      ", but found " + Error.formatYellow(actual)
    Error(message, position, code)
  }
}

case object BinaryOperatorError {
  def expectation(
    operator: String,
    expected: String,
    actual: String,
    position: (Int, Int),
    side: String,
    code: Int = 200
  ): Error = {
    val message = "Pre-defined function " + operator + " expected " + Error.formatYellow(expected) + " for " + side +
      " operand, but found " + Error.formatYellow(actual)
    Error(message, position, code)
  }

  def comparison(operator: String, leftType: String, rightType: String, position: (Int, Int), code: Int = 200): Error =
    Error(
      "Cannot compare " + Error.formatYellow(leftType) + " and " + Error.formatYellow(rightType) +
        " using pre-defined function " + operator,
      position,
      code
    )
}

case object FunctionCallError {
  def expectation(name: String, expected: String, actual: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Type mismatch in function call for user-defined function \"" + name +
      "\":\n\t  Function expected call type " + Error.formatYellow(expected) + ", but found " +
      Error.formatYellow(actual)
    Error(message, position, code)
  }

  def undefined(name: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Invalid function call to undefined function " + Error.formatYellow(name)
    Error(message, position, code)
  }

  def invalid(name: String, actual: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Invalid function call to alleged user-defined function \"" + name +
      "\":\n\t  Actual type of " + name + " is " + Error.formatYellow(actual)
    Error(message, position, code)
  }
}

object Error {
  def formatYellow(message: String): String = Console.YELLOW + message + Console.RESET
  def formatRed(message: String): String = Console.RED + message + Console.RESET
}
