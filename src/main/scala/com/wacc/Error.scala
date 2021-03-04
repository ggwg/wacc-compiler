package com.wacc

case class Error(message: String, position: (Int, Int) = Error.defaultPosition, code: Int = Error.semanticCode) {
  val getMessage: String =
    code match {
      case Error.semanticCode | Error.syntaxCode =>
        Error.formatRed(
          "(Error Code " + code + ")"
        ) + " Semantic error on line " + position._1 + ", column " + position._2 + ":\n\t- " + message
      case _ => Error.formatRed("(Runtime Error)" + ":\n\t- " + message)
    }

  def throwError(): Unit = {
    Console.out.println(getMessage)
  }
}

object Error {
  /* Error Codes & Misc. Constants */
  val defaultPosition: (Int, Int) = (0, 0)
  val semanticCode = 200
  val syntaxCode = 201
  val runtimeCode = 202

  /* Message Formatter Functions */
  def formatYellow(message: String): String = Console.YELLOW + message + Console.RESET
  def formatRed(message: String): String = Console.RED + message + Console.RESET
}

/* Syntax Error Handlers */
case object BoundError {
  def exceed(_type: String, position: (Int, Int), isMinimum: Boolean): Error = {
    val side = if (isMinimum) "Minimum" else "Maximum"
    val message = side + " value of bound of type " + Error.formatYellow(_type) + " exceeded!"
    Error(message, position, Error.syntaxCode)
  }
}

/* Semantic Error Handlers */
case object UnaryOperatorError {
  def expectation(operator: String, expected: String, actual: String, position: (Int, Int)): Error = {
    val message = "Pre-defined function " + operator + " expected " + Error.formatYellow(expected) +
      ", but found " + Error.formatYellow(actual)
    Error(message, position)
  }
}

case object BinaryOperatorError {
  def expectation(operator: String, expected: String, actual: String, position: (Int, Int), side: String): Error = {
    val message = "Pre-defined function " + operator + " expected " + Error.formatYellow(expected) + " for " + side +
      " operand, but found " + Error.formatYellow(actual)
    Error(message, position)
  }

  def comparison(operator: String, leftType: String, rightType: String, position: (Int, Int)): Error = {
    val message = "Cannot compare " + Error.formatYellow(leftType) + " and " + Error.formatYellow(rightType) +
      " using pre-defined function " + operator
    Error(message, position)
  }
}

case object FunctionCallError {
  def expectation(name: String, expected: String, actual: String, position: (Int, Int)): Error = {
    val message = "Type mismatch in function call for user-defined function \"" + name +
      "\":\n\t  Function expected call type " + Error.formatYellow(expected) + ", but found " +
      Error.formatYellow(actual)
    Error(message, position)
  }

  def undefined(name: String, position: (Int, Int)): Error = {
    val message = "Invalid function call to undefined function " + Error.formatYellow(name)
    Error(message, position)
  }

  def invalid(name: String, actual: String, position: (Int, Int)): Error = {
    val message = "Invalid function call to alleged user-defined function \"" + name +
      "\":\n\t  Actual type of " + name + " is " + Error.formatYellow(actual)
    Error(message, position)
  }
}

case object ArrayElementError {
  def expectation(name: String, expected: String, actual: String, position: (Int, Int)): Error = {
    val message = "Invalid index in attempt to access element of array \"" + name +
      "\":\n\t  Array index expected type " + Error.formatYellow(expected) + ", but found " + Error.formatYellow(actual)
    Error(message, position)
  }

  def missing(name: String, position: (Int, Int)): Error = {
    val message = "Array index not specified in attempt to access array " + name
    Error(message, position)
  }
}

case object IdentifierError {
  def undefined(name: String, position: (Int, Int)): Error = {
    val message = "Undefined identifier \"" + name + "\""
    Error(message, position)
  }
}

case object ArrayLiterError {
  def expectation(expected: String, actual: String, position: (Int, Int)): Error = {
    val message = "Type mismatch in array assignment:\n\t  Array elements must be of type " +
      Error.formatYellow(expected) + ", but found " + Error.formatYellow(actual)
    Error(message, position)
  }
}

case object PairElementError {
  def expectation(expected: String, actual: String, isFirst: Boolean, position: (Int, Int)): Error = {
    val function = if (isFirst) "fst" else "snd"
    val message = "Pre-defined function " + function + " expected " + Error.formatYellow(expected) + ", but found " +
      Error.formatYellow(actual)
    Error(message, position)
  }
}

/* Runtime Error Handlers */
sealed trait RuntimeErrorTrait {
  val label: String
  val message: String
  def errorMessage: String = Error(message, code = Error.runtimeCode).getMessage
}
sealed trait ArrayIndexErrorTrait extends RuntimeErrorTrait

case object RuntimeError extends RuntimeErrorTrait {
  val message = ""
  val label = "p_throw_runtime_error"
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

case object ArrayIndexError extends ArrayIndexErrorTrait {
  val message = ""
  val label = "p_check_array_bounds"
}

case object ArrayIndexNegativeError extends ArrayIndexErrorTrait {
  val message = "Trying to access a negative index"
  val label: String = ArrayIndexError.label
}

case object ArrayIndexBoundsError extends ArrayIndexErrorTrait {
  val message = "Trying to access an index out of array bounds"
  val label: String = ArrayIndexError.label
}
