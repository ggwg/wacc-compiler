package com.wacc

import scala.collection.mutable

case class Program(functions: List[Function], body: Statement) extends ASTNodeVoid {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduceOption((left, right) => left + right)
    .getOrElse("") + body.toString + "end"

  override def check(symbolTable: SymbolTable)(implicit errors : mutable.ListBuffer[Error]): Unit = {
    // TODO: check function name and return type, slide 31
    println("CHECKED INSIDE PROGRAM")
    val programSymbolTable = new SymbolTable(symbolTable)
    functions.foreach { func =>
      func.check(programSymbolTable)
    }
    body.check(programSymbolTable)
  }
}

/* Check done */
/* Function declaration - see P31 of semantic analysis slides */
case class Function(returnType: Type, name: Identifier, parameters: Option[ParameterList], body: Statement)
    extends ASTNodeVoid {
  override def toString: String =
    returnType.toString + " " + name.toString + "(" +
      parameters.getOrElse("").toString + ") is\n" + body.toString + "end\n"

  override def check(symbolTable: SymbolTable)(implicit errors : mutable.ListBuffer[Error]): Unit = {
    println("CHECKED INSIDE FUNCTION")
    val pos = (39, 15)
    // Check function name and return type:
    var F = symbolTable.lookup(name.identifier)
    if (!F.isEmpty) {
      errors += DefaultError("Function name " + name.identifier + "already defined in scope.", pos)
    } else {
      // Add to symbol table
      symbolTable.add(name.identifier, returnType, this)
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
  override def check(symbolTable: SymbolTable)(implicit errors : mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE PARAMETER-LIST CHECK")
    for (parameter <- parameters) {
      parameter.check(symbolTable)
    }
  }
}

case class Parameter(parameterType: Type, identifier: Identifier) extends ASTNodeVoid {
  override def toString: String =
    parameterType.toString + " " + identifier.toString

  // TODO:
  override def check(symbolTable: SymbolTable)(implicit errors : mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE PARAMETER CHECK")
    var parameterInfo = symbolTable.lookup(identifier.identifier)
    if (parameterInfo.isEmpty) {
      symbolTable.add(identifier.identifier, parameterType, this)
    } else {
      var pos = (39,15)
      errors += DefaultError("Error - parameter " + identifier.identifier + "already defined in scope.", pos)
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
  val build: (Parameter, List[Parameter]) => ParameterList = (p, ps) => ParameterList(p :: ps)
}

object Parameter {
  val build: (Type, Identifier) => Parameter = Parameter(_, _)
}
