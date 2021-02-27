package com.wacc

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

    /* Compile the program body */
    newState = body.compileNewScope(newState)(instructions)

    /* Set the exit code to 0 */
    instructions += LOAD(Register0, ImmediateLoad(0))

    /* Pop the PC */
    instructions += PopPC()

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

/* Check done */
/* Function declaration - see P31 of semantic analysis slides */
case class Function(returnType: Type, name: Identifier, parameters: Option[ParameterList], body: Statement)(
  position: (Int, Int)
) extends ASTNodeVoid {
  override def toString: String =
    returnType.toString + " " + name.toString + "(" +
      parameters.getOrElse("").toString + ") is\n" + body.toString + "end\n"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* If we have parameters */
    if (parameters.isDefined) {
      val params = parameters.get

      /* Process them */
      for (param <- params.parameters) {
        /* Parameter size */
        val size = param.parameterType.getSize

        /* Update the state */
        newState = newState.copy(
          spOffset = newState.spOffset + size,
          varDic = newState.varDic + (param.identifier.identifier -> (newState.spOffset + size))
        )
      }
    }

    /* Add the function label */
    instructions += StringLabel("f_" + name.identifier)

    /* Compile the function body */
    instructions += PushLR()
    newState = newState.copy(spOffset = newState.spOffset + 4)

    newState = body.compileNewScope(newState)(instructions)

    instructions += PopPC()
    newState = newState.copy(spOffset = newState.spOffset - 4)

    /* Add the ltorg directive */
    instructions += LtorgDirective()

    /* Reset the state to where it was initially */
    newState.copy(spOffset = state.spOffset)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    // Check that the function returns:
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
