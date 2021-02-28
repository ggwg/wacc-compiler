package com.wacc

import com.wacc.Function.initSP
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Program(functions: List[Function], body: Statement)(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduceOption((left, right) => left + right)
    .getOrElse("") + body.toString + "end"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* Compile all the functions */
    for (function <- functions) {
      newState = function.compile(newState)
    }

    /* Add the main label */
    instructions += StringLabel("main")

    /* Push the LR */
    instructions += PushLR()
    newState = newState.copy(spOffset = newState.spOffset + 4)

    /* Compile the program body */
    newState = body.compileNewScope(newState)

    /* Set the exit code to 0 */
    instructions += LOAD(Register0, ImmediateLoad(0))

    /* Pop the PC */
    instructions += PopPC()
    newState = newState.copy(spOffset = newState.spOffset - 4)

    /* Add the ltorg directive */
    instructions += LtorgDirective()

    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    functions.foreach { func =>
      val F = symbolTable.lookup(func.name.identifier)
      if (F.isDefined) {
        errors +=
          Error("Function " + func.name.identifier + " conflicts with another variable in the current scope.", getPos())
      }

      symbolTable.add(func.name.identifier, func.returnType, func)
    }
    functions.foreach { func =>
      func.check(symbolTable)
    }
    val bodySymbolTable = new SymbolTable(symbolTable)
    body.check(bodySymbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* Function declaration - see P31 of semantic analysis slides */
case class Function(returnType: Type, name: Identifier, parameters: Option[ParameterList], body: Statement)(
  position: (Int, Int)
) extends ASTNodeVoid {
  override def toString: String =
    returnType.toString + " " + name.toString + "(" +
      parameters.getOrElse("").toString + ") is\n" + body.toString + "end\n"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* Compile the parameters */
    if (parameters.isDefined) {
      newState = parameters.get.compile(newState)
    }

    /* Add the function label */
    instructions += StringLabel("f_" + name.identifier)

    /* Push the LR */
    instructions += PushLR()
    newState = newState.copy(spOffset = newState.spOffset + 4)

    /* Compile the function body. We add a special entry in the dictionary so that when we
       return we know where to reposition the stack pointer */
    newState = newState.copy(varDic = newState.varDic + (initSP -> newState.spOffset))
    newState = body.compileNewScope(newState)(instructions)
    newState = newState.copy(varDic = newState.varDic - initSP)

    /* Pop the PC */
    instructions += PopPC()
    newState = newState.copy(spOffset = newState.spOffset - 4)

    /* Add the ltorg directive */
    instructions += LtorgDirective()

    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* Check that the function returns: */
    if (!body.exitable()) {
      errors += Error("Function " + name.identifier + " may terminate without return", getPos(), 100)
    }

    val functionSymbolTable = new SymbolTable(symbolTable, true, returnType)

    if (parameters.isDefined) {
      parameters.get.check(functionSymbolTable)
    }

    body.check(functionSymbolTable)

  }

  override def getPos(): (Int, Int) = position
}

case class ParameterList(parameters: List[Parameter])(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String =
    parameters
      .map(_.toString)
      .reduceOption((left, right) => left + ", " + right)
      .getOrElse("")

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* Process each parameter */
    for (param <- parameters) {
      newState = param.compile(newState)
    }

    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    for (parameter <- parameters) {
      parameter.check(symbolTable)
    }
  }

  override def getPos(): (Int, Int) = position
}

case class Parameter(parameterType: Type, identifier: Identifier)(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String =
    parameterType.toString + " " + identifier.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Parameter size */
    val size = parameterType.getSize

    /* Update the state */
    state.copy(
      spOffset = state.spOffset + size,
      varDic = state.varDic + (identifier.identifier -> (state.spOffset + size))
    )
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    symbolTable.dictionary.updateWith(identifier.identifier)({
      case Some(x) =>
        errors +=
          Error("Function parameter " + identifier.identifier + " conflicts with another parameter in scope.", getPos())
        Some(x)
      case None => Some(parameterType, this)
    })
  }

  override def getPos(): (Int, Int) = position
}

object Program {
  def apply(funs: Parsley[List[Function]], body: Parsley[Statement]): Parsley[Program] =
    pos <**> (funs, body).map(Program(_, _))
}

object Function {
  val initSP: String = "-initSPOffset"

  def apply(
    returnType: Parsley[Type],
    name: Parsley[Identifier],
    params: Parsley[Option[ParameterList]],
    body: Parsley[Statement]
  ): Parsley[Function] = pos <**> (returnType, name, params, body).map(Function(_, _, _, _))
}

object ParameterList {
  def apply(param: Parsley[Parameter], params: Parsley[List[Parameter]]): Parsley[ParameterList] =
    pos <**> (param, params).map((p, ps) => ParameterList(p :: ps))
}

object Parameter {
  def apply(paramType: Parsley[Type], name: Parsley[Identifier]): Parsley[Parameter] =
    pos <**> (paramType, name).map(Parameter(_, _))
}
