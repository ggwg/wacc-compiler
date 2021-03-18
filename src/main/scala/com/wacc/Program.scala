package com.wacc

import com.wacc.Function.initSP
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Program(imports: List[Import], functions: List[Function], body: Statement)(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduceOption((left, right) => left + right)
    .getOrElse("") + body.toString + "end"

  override def removeUnreachableStatements(): Program = {
    Program(imports, functions.map(_.removeUnreachableStatements()), body.removeUnreachableStatements())(position)
  }

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    for (imp <- imports) {
      newState = imp.compile(newState)
    }

    for (function <- functions) {
      /* Overloaded Functions: Get the generated label for all functions */
      /* Add function to assembler state */
      newState = newState.putFunction(function.name.identifier, function.thisFunctionType)
    }

    /* Compile all the functions */
    for (function <- functions) {
      newState = function.compile(newState)
    }

    /* Add the main label and push the LR */
    instructions ++= List(StringLabel("main"), PushLR())
    newState = newState.copy(spOffset = newState.spOffset + 4)

    /* Compile the program body */
    newState = body.compileNewScope(newState)

    /* Set the exit code to 0, pop the PC, and add the ltorg directive */
    instructions ++= List(LOAD(Register0, ImmediateLoad(0)), PopPC(), LtorgDirective())
    newState.copy(spOffset = newState.spOffset - 4)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    // Check each of the files at the imports
    for (imp <- imports) {
      imp.check(symbolTable)
    }

    functions.foreach { func =>
      // TODO: Create functionType object for function
      val functionAdded = symbolTable.addFunction(func.name.identifier, func.getType(symbolTable))
      if (!functionAdded) {
        errors += Error("ERROR", getPos(), Error.semanticCode)
      }

//      val F = symbolTable.lookup(func.name.identifier)
//      if (F.isDefined) {
//        /* TODO: ALLOW FUNCTION OVERLOADING! */
//        symbolTable.add(func.name.identifier, func.returnType, func)
////        errors +=
////          Error("Function " + func.name.identifier + " conflicts with another variable in the current scope.", getPos())
//      } else {
//        /* TODO: FUNCTION OVERLOADING (DEFAULT CASE) */
//        symbolTable.add(func.name.identifier, func.returnType, func)
//      }

    }
    functions.foreach { func =>
      func.check(symbolTable)
    }
    val bodySymbolTable = new SymbolTable(symbolTable)
    body.check(bodySymbolTable)
  }

  override def getPos(): (Int, Int) = position
}

case class Import(fileName: Identifier)(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String = "import " + fileName

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    println("importDic:" + state.importDic)
    instructions ++= state.getImport(fileName.identifier)
    // Error message if import not found
    state
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    // Check the imported program itself recursively.
    println(fileName.identifier)
  }

  override def getPos(): (Int, Int) = position
}

/* Function declaration - see P31 of semantic analysis slides */
case class Function(returnType: Type, name: Identifier, parameters: Option[ParameterList], body: Statement)(
  position: (Int, Int)
) extends ASTNodeVoid {
  /* Stores the function type of the program. The function type will be defined after semantic analysis
     (.check() call) */
  var thisFunctionType: Type = NotAType()
  override def toString: String =
    returnType.toString + " " + name.toString + "(" +
      parameters.getOrElse("").toString + ") is\n" + body.toString + "end\n"

  override def removeUnreachableStatements(): Function = {
    Function(returnType, name, parameters, body.removeUnreachableStatements())(position)
  }

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* Compile the parameters */
    if (parameters.isDefined) {
      newState = parameters.get.compile(newState)
    }

    /* Call getFunctionLabel to get the string label. */
    val functionLabel = newState.getFunctionLabel(name.identifier, thisFunctionType)

    /* Add the function label and push the LR */
    instructions ++= List(StringLabel("f_" + functionLabel), PushLR())
    newState = newState.copy(spOffset = newState.spOffset + 4)

    /* Compile the function body. We add a special entry in the dictionary so that when we
       return we know where to reposition the stack pointer */
    newState = newState.copy(varDic = newState.varDic + (initSP -> newState.spOffset))
    newState = body.compileNewScope(newState)
    newState = newState.copy(varDic = newState.varDic - initSP)

    /* Pop the PC and add the ltorg directive */
    instructions ++= List(PopPC(), LtorgDirective())
    newState.copy(spOffset = newState.spOffset - newState.declaredSize - 4, declaredSize = 0)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Check that the function returns if it does not return a void type: */
    returnType match {
      case VoidType() => ()
      /* No need to check that function returns anything - ignore it */
      case _ =>
        /* Check that the function returns: */
        if (!body.exitable()) {
          errors += Error("Function " + name.identifier + " may terminate without return", getPos(), Error.syntaxCode)
        }
    }

    val functionSymbolTable = new SymbolTable(symbolTable, true, returnType)

    if (parameters.isDefined) {
      parameters.get.check(functionSymbolTable)
    }

    body.check(functionSymbolTable)
    thisFunctionType = getType(symbolTable)
  }

  override def getType(symbolTable: SymbolTable): Type = {
    val expectedParams = {
      parameters match {
        case Some(list: ParameterList) => Some(list.parameters.map(parameter => parameter.parameterType))
        case None                      => None
      }
    }
    FunctionType(returnType, expectedParams)
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
  override def toString: String = parameterType.toString + " " + identifier.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Parameter size */
    val size = parameterType.getSize

    /* Update the state */
    state.copy(
      spOffset = state.spOffset + size,
      varDic = state.varDic + (identifier.identifier -> (state.spOffset + size)),
      declaredSize = state.declaredSize + size
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
  def apply(imports: Parsley[List[Import]], funs: Parsley[List[Function]], body: Parsley[Statement]): Parsley[Program] =
    pos <**> (imports, funs, body).map(Program(_, _, _))
}

object Import {
  def apply(fileName: Parsley[Identifier]): Parsley[Import] = pos <**> fileName.map(Import(_))
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
