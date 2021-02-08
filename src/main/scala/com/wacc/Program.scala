package com.wacc

case class Program(functions: List[Function], body: Statement) extends ASTNode {
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

case class Function(
    functionType: Type,
    functionName: Identifier,
    parameters: Option[ParameterList],
    body: Statement
) extends ASTNode {
  override def toString: String =
    functionType.toString + " " + functionName.toString + "(" + parameters
      .getOrElse("")
      .toString + ") is\n" + body.toString + "end\n"

  override def check(symbolTable: SymbolTable): Unit = {
    // check function name and return type, slide 31
    println("CHECKED INSIDE FUNCTION")
  }
}

case class ParameterList(parameters: List[Parameter]) extends ASTNode {
  override def toString: String =
    parameters
      .map(_.toString)
      .reduceOption((left, right) => left + ", " + right)
      .getOrElse("")

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PARAMETER-LIST CHECK")
  }
}

case class Parameter(parameterType: Type, identifier: Identifier)
    extends ASTNode {
  override def toString: String =
    parameterType.toString + " " + identifier.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PARAMETER-LIST CHECK")
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
