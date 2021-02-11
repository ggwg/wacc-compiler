package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable

case class Program(functions: List[Function], body: Statement) extends ASTNodeVoid {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduceOption((left, right) => left + right)
    .getOrElse("") + body.toString + "end"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    // TODO: check function name and return type, slide 31
    println("CHECKED INSIDE PROGRAM")
    functions.foreach { func => func.check(symbolTable) }
    body.check(symbolTable)
  }
}

/* Check done */
/* Function declaration - see P31 of semantic analysis slides */
case class Function(returnType: Type, name: Identifier, parameters: Option[ParameterList], body: Statement)
    extends ASTNodeVoid {
  override def toString: String =
    returnType.toString + " " + name.toString + "(" +
      parameters.getOrElse("").toString + ") is\n" + body.toString + "end\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("CHECKED INSIDE FUNCTION")
    val pos = (0, 0)
    // Check function name and return type:
    val F = symbolTable.lookup(name.identifier)
    if (!F.isEmpty) {
      errors +=
        DefaultError("Function " + name.identifier + " conflicts with another variable in the current scope.", pos)
      println("DIDNT ADD TO SYMBOL TABLE")
    } else {
      // Add to symbol table
      symbolTable.add(name.identifier, returnType, this)
      var functionSymbolTable = new SymbolTable(symbolTable)
      if (!parameters.isEmpty) {
        parameters.get.check(functionSymbolTable)
      }
    }
    List.empty
  }
}

case class ParameterList(parameters: List[Parameter]) extends ASTNodeVoid {
  override def toString: String =
    parameters
      .map(_.toString)
      .reduceOption((left, right) => left + ", " + right)
      .getOrElse("")

  // TODO:
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
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
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE PARAMETER CHECK")
    var pos = (0, 0)
    var parameterInfo = symbolTable.lookup(identifier.identifier)
    if (parameterInfo.isEmpty) {
      symbolTable.add(identifier.identifier, parameterType, this)
    } else {
      errors +=
        DefaultError("Function parameter " + identifier.identifier + " conflicts with another parameter in scope.", pos)
    }
  }
}

object Program {
  def apply(funs: Parsley[List[Function]], body: Parsley[Statement]): Parsley[Program] = (funs, body).map(Program(_, _))
}

object Function {
  def apply(
    returnType: Parsley[Type],
    name: Parsley[Identifier],
    params: Parsley[Option[ParameterList]],
    body: Parsley[Statement]
  ): Parsley[Function] = (returnType, name, params, body).map(Function(_, _, _, _))
}

object ParameterList {
  def apply(param: Parsley[Parameter], params: Parsley[List[Parameter]]): Parsley[ParameterList] =
    (param, params).map((p, ps) => ParameterList(p :: ps))
}

object Parameter {
  def apply(paramType: Parsley[Type], name: Parsley[Identifier]): Parsley[Parameter] =
    (paramType, name).map(Parameter(_, _))
}
