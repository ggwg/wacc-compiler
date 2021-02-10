package com.wacc

case class Program(functions: List[Function], body: Statement) extends ASTNodeVoid {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduceOption((left, right) => left + right)
    .getOrElse("") + body.toString + "end"

  override def check(symbolTable: SymbolTable): Unit = {
    // TODO: check function name and return type, slide 31

    println("CHECKED INSIDE PROGRAM")

    val newSymbolTable = new SymbolTable(symbolTable)

    functions.foreach { func =>
      func.check(newSymbolTable)
    }
  }
}

/* Check done */
/* Function declaration - see P31 of semantic analysis slides */
case class Function(
    functionType: Type,
    functionName: Identifier,
    parameters: Option[ParameterList],
    body: Statement
) extends ASTNodeVoid {
  override def toString: String =
    functionType.toString + " " + functionName.toString + "(" + parameters
      .getOrElse("")
      .toString + ") is\n" + body.toString + "end\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("CHECKED INSIDE FUNCTION")
    // Check function name and return type:
    var F = symbolTable.lookup(functionName.identifier)
    if (!F.isEmpty) {
      println("Error - already declared identifier " + functionName.identifier)
    } else {
      // Add to symbol table
      symbolTable.add(functionName.identifier, functionType, this)
      var functionSymbolTable = new SymbolTable(symbolTable)
      if (!parameters.isEmpty) {
        parameters.get.check(functionSymbolTable)
      }
    }
  }
}

case class ParameterList(parameters: List[Parameter]) extends ASTNodeVoid {
  override def toString: String =
    parameters
      .map(_.toString)
      .reduceOption((left, right) => left + ", " + right)
      .getOrElse("")

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PARAMETER-LIST CHECK")
    for (parameter <- parameters) {
      parameter.check(symbolTable)
    }
  }
}

case class Parameter(parameterType: Type, identifier: Identifier)
    extends ASTNodeVoid {
  override def toString: String =
    parameterType.toString + " " + identifier.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PARAMETER CHECK")
    var parameterInfo = symbolTable.lookup(identifier.identifier)
    if (parameterInfo.isEmpty) {
      symbolTable.add(identifier.identifier, parameterType, this)
    } else {
      println("Error - parameter already defined in scope.")
    }
  }
}

object Program {
  val build: (List[Function], Statement) => Program = Program(_, _)
}

object Function {
  val build: (Type, Identifier, Option[ParameterList], Statement) => Function =
    Function(_, _, _, _)
}

object ParameterList {
  val build: (Parameter, List[Parameter]) => ParameterList = (p, ps) =>
    ParameterList(p :: ps)
}

object Parameter {
  val build: (Type, Identifier) => Parameter = Parameter(_, _)
}
