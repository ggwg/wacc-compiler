package com.wacc

case class Error(message: String, position: (Int, Int) = (0, 0), code: Int = 200) {
  val getMessage: String = {
    code match {
      case 200 | 201 =>
        Error.formatRed(
          "(Error Code " + code + ")"
        ) + " Semantic error on line " + position._1 + ", column " + position._2 + ":\n\t- " + message
      case _ => Error.formatRed("(Runtime Error)" + ":\n\t- " + message)
    }
  }

  def throwError(): Unit = {
    Console.out.println(getMessage)
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

case object ArrayElementError {
  def expectation(name: String, expected: String, actual: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Invalid index in attempt to access element of array \"" + name +
      "\":\n\t  Array index expected type " + Error.formatYellow(expected) + ", but found " + Error.formatYellow(actual)
    Error(message, position, code)
  }

  def missing(name: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Array index not specified in attempt to access array " + name
    Error(message, position, code)
  }
}

case object IdentifierError {
  def undefined(name: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Undefined identifier \"" + name + "\""
    Error(message, position, code)
  }
}

case object ArrayLiterError {
  def expectation(expected: String, actual: String, position: (Int, Int), code: Int = 200): Error = {
    val message = "Type mismatch in array assignment:\n\t  Array elements must be of type " +
      Error.formatYellow(expected) + ", but found " + Error.formatYellow(actual)
    Error(message, position, code)
  }
}

case object PairElementError {
  def expectation(expected: String, actual: String, isFirst: Boolean, position: (Int, Int), code: Int = 200): Error = {
    val function = if (isFirst) "fst" else "snd"
    val message = "Pre-defined function " + function + " expected " + Error.formatYellow(expected) + ", but found " +
      Error.formatYellow(actual)
    Error(message, position, code)
  }
}

sealed trait RuntimeErrorTrait {
  val code = 202
  val label: String
  val message: String
  def errorMessage: String = Error(message, code = code).getMessage
}

case object RunTimeError extends RuntimeErrorTrait {
  val label = "p_throw_runtime_error"
  val message = ""
}

case object OverflowError extends RuntimeErrorTrait {
  val message = "Trying to store too small/large result in a 4-byte signed-integer"
  val label = "p_throw_overflow_error"
}

case object DivideByZeroError extends RuntimeErrorTrait {
  val message = "Trying to divide or modulo by zero"
  val label = "p_check_divide_by_zero"
}

case object NullDereferenceError extends RuntimeErrorTrait {
  val message = "Trying to dereference a null reference"
  val label = "p_check_null_pointer"
}

case object FreeNullPairError extends RuntimeErrorTrait {
  val message = "Trying to free a null reference in pair context"
  val label = "p_free_pair"
}

case object FreeNullArrayError extends RuntimeErrorTrait {
  val message = "Trying to free a null reference in array context"
  val label = "p_free_array"
}

case class ArrayIndexError() extends RuntimeErrorTrait {
  val label = "p_check_array_bounds"
  val message: String = ""
}

case object ArrayIndexNegativeError extends ArrayIndexError {
  override val message = "Trying to access a negative index"
}

case object ArrayIndexBounds extends ArrayIndexError {
  override val message = "Trying to access an index out of array bounds"
}

object Error {
  def formatYellow(message: String): String = Console.YELLOW + message + Console.RESET
  def formatRed(message: String): String = Console.RED + message + Console.RESET
}
