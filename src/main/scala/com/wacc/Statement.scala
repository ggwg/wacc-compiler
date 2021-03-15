package com.wacc

import parsley.Parsley
import parsley.Parsley.{label, pos, select}
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Statement extends ASTNodeVoid {
  /* Returns true if the statement ends with a return or exit no matter what 'branches' it takes,
     false otherwise. */
  def exitable(): Boolean = {
    this match {
      case Exit(_) | Return(_) => true
      case BeginEnd(statement) => statement.exitable()
      case If(_, trueStatement, falseStatement) =>
        trueStatement.exitable() && falseStatement.exitable()
      case StatementSequence(_, statement) => statement.exitable()
      case While(_, statement)             => statement.exitable()
      case _                               => false
    }
  }

  /* Given the 'parent' state, this function compiles the scope statement with a scope state, then
     ensures the compilation can proceed after exiting the scope statement. */
  def compileNewScope(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Create a new state for the new scope, compile the body with the new scope, reset the SP to where we were
       initially and restore the state to hold the correct fields.  */
    var scopeState = state.newScopeState
    scopeState = this.compile(scopeState)
    instructions += ADD(RegisterSP, RegisterSP, ImmediateNumber(scopeState.declaredSize))
    scopeState.fromScopeToInitialState(state)
  }
}

case class IdentifierDeclaration(identType: Type, name: Identifier, assignmentRight: AssignmentRight)(
  position: (Int, Int)
) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the right hand side */
    val resultReg = state.getResultRegister
    var newState = assignmentRight.compile(state)

    /* Store the identifier on the stack */
    val size = identType.getSize
    instructions ++= List(
      SUB(RegisterSP, RegisterSP, ImmediateNumber(size)),
      STORE(resultReg, RegisterLoad(RegisterSP), size == 1)
    )

    /* Update the state to reflect the change */
    val newSPOffset = newState.spOffset + size
    val newVarDic = newState.varDic + (name.identifier -> newSPOffset)
    val newDeclaredSize = newState.declaredSize + size
    newState = newState.copy(spOffset = newSPOffset, varDic = newVarDic, declaredSize = newDeclaredSize)

    /* Mark the result register as free */
    newState.copy(freeRegs = resultReg :: newState.freeRegs)
  }

  override def toString: String =
    identType.toString + " " + name.toString + " = " + assignmentRight.toString + "\n"

  /* Check if identifier is already defined in the symbol table (current, not parent)
   * If so, then record error because variable names must not class with existing
   * variable names or any keyword. Extract type of identType, then we check if this
   * is the same type as assignmentRight. If so, add it to symbol table. Else, error.
   */
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    symbolTable.dictionary.updateWith(name.identifier)({
      case Some(x) =>
        errors += Error(
          "Variable declaration " + name.identifier +
            " already defined in current scope.",
          getPos()
        )
        Some(x)
      case None =>
        /* If left type == right type, then we can add it to dictionary. */
        /* First we look up if RHS is a function in the functionDic (in SymbolTable) */
        assignmentRight match {
          case functionIdentifier: Identifier =>
            identType match {
              case expectedFunctionType @ FunctionType(_, _) =>
                symbolTable.lookupAllFunction(functionIdentifier.identifier, expectedFunctionType) match {
                  case f @ FunctionType(_, _) =>
                    Some((f, assignmentRight))
                  case _ => assignRHS(identType, assignmentRight, symbolTable)
                }
              case _ => assignRHS(identType, assignmentRight, symbolTable)
            }
          /* If RHS is not identifier, then assign LHS to RHS */
          case _ => assignRHS(identType, assignmentRight, symbolTable)
        }
    })
  }

  private def assignRHS(identType: Type, assignmentRight: AssignmentRight, symbolTable: SymbolTable)
                       (implicit errors: mutable.ListBuffer[Error])
                       : Option[(Type, ASTNodeVoid)] = {
    /* If its not in the function dictionary, then it check regular ST */
    if (identType.sameTypes(assignmentRight, symbolTable)) {
      assignmentRight.check(symbolTable)
      Some((identType, assignmentRight))
    } else {
      errors += Error(
        "Invalid types in identifier assignment. Got: " +
          assignmentRight.getType(symbolTable) + ", Expected: " + identType,
        getPos()
      )
      None
    }
  }

  override def getPos(): (Int, Int) = position
}

case class Assignment(assignmentLeft: AssignmentLeft, assignmentRight: AssignmentRight)(position: (Int, Int))
    extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the pointer of the thing that will be assigned */
    val assignmentRegister = state.getResultRegister
    var newState = assignmentRight.compile(state)

    /* Compile the value to be assigned to the left side and assign the value */
    val assignPointer = newState.getResultRegister
    newState = assignmentLeft.compileReference(newState)
    instructions += STORE(assignmentRegister, RegisterLoad(assignPointer), assignmentLeft.getLeftType.getSize == 1)

    /* Mark the registers as being usable */
    newState.copy(freeRegs = assignmentRegister :: assignPointer :: newState.freeRegs)
  }
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Note: for assignment of functions:
    *   1. Get left type of function - if it is a FunctionType then:
    *   2. Check if RHS is in the FunctionDictionary in the SymbolTable */
    /* Check that assignment-left type is same as return type of assignment-right */
    assignmentLeft match {
      case Identifier(identifier) =>
        if (symbolTable.identifierIsFunction(identifier)) {
          errors += Error("Function identifier " + identifier + " cannot be assigned", getPos())
          return
        }
      case _ => ()
    }
    /* Check if LHS is referring to a function type */
    assignmentLeft.getType(symbolTable) match {
      case f @ FunctionType(_, _) =>
        /* Check if RHS is an identifier in the FunctionDictionary, and see if there is a matching type for it. */
        assignmentRight match {
          case functionIdentifier: Identifier =>
            /* Check if RHS is actually referring to a defined function */
            symbolTable.lookupAllFunction(functionIdentifier.identifier, f) match {
              case FunctionType(_, _) =>
                return
              case _ => ()
            }
          case _ => ()
        }
    }
    if (!assignmentLeft.getType(symbolTable).unifies(assignmentRight.getType(symbolTable))) {
      errors += Error(
        "Type missmatch for assignment. Got: " + assignmentRight.getType(symbolTable) + ", Expected: " + assignmentLeft
          .getType(symbolTable),
        getPos()
      )
      return
    }
    assignmentLeft.check(symbolTable)
    assignmentRight.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

case class BeginEnd(statement: Statement)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    statement.compileNewScope(state)
  }

  override def toString: String = "begin\n" + statement.toString + "end\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Create new scope for Symbol Table and recursively call check */
    val beginEndSymbolTable = new SymbolTable(symbolTable)
    statement.check(beginEndSymbolTable)
  }

  override def getPos(): (Int, Int) = position
}

case class Exit(expression: Expression)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression and store it in r0; then call the exit function */
    val resultReg = state.getResultRegister
    val newState = expression.compile(state)
    instructions ++= List(MOVE(Register0, resultReg), BRANCHLINK("exit"))

    /* Mark the result register as usable */
    newState.copy(freeRegs = resultReg :: newState.freeRegs)
  }

  override def toString: String = "exit " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    val pos = getPos()
    if (!expression.getType(symbolTable).unifies(IntType())) {
      errors += Error("Exit expression not type Int", pos)
    }
    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* Memory Free Statements: A memory free statement ‘free’ is used to free the heap memory allocated for a pair or array
   and its immediate content. The statement is given an expression that must be of type ‘pair(T1, T2)’ or ‘T[]’
   (for some T, T1, T2). The expression must evaluate to a valid reference to a pair or array, otherwise a segmentation
   fault will occur at runtime. If the reference is valid, then the memory for each element of the pair/array is freed,
   so long as the element is not a reference to another pair or another array (i.e. free is not recursive). Then the
   memory that stores the pair/array itself is also freed. */
case class Free(expression: Expression)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression */
    val resultReg = state.getResultRegister
    var newState = expression.compile(state)

    instructions += MOVE(Register0, resultReg)
    expression.getExpressionType match {
      case ArrayType(_) =>
        /* Free the array memory */
        newState = newState.putMessageIfAbsent(FreeNullArrayError.errorMessage)
        instructions += BRANCHLINK(FreeNullArrayError.label)
        newState = newState.copy(p_free_array = true, p_throw_runtime_error = true, freeRegs = resultReg :: newState.freeRegs)
      case PairType(_, _) =>
        /* Free the pair memory */
        newState = newState.putMessageIfAbsent(FreeNullPairError.errorMessage)
        instructions += BRANCHLINK(FreeNullPairError.label)
        newState = newState.copy(p_free_pair = true, p_throw_runtime_error = true, freeRegs = resultReg :: newState.freeRegs)
    }
    newState
  }

  override def toString: String = "free " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    expression.getType(symbolTable) match {
      case PairType(_, _) | ArrayType(_) =>
        expression.check(symbolTable)
      case _ =>
        errors += Error("Attempted to free non pair or array type.", position)
    }
  }

  override def getPos(): (Int, Int) = position
}

case class If(condition: Expression, trueStatement: Statement, falseStatement: Statement)(position: (Int, Int))
    extends Statement {
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val labelPrefix = "L"

    /* Compile the if condition */
    val conditionReg = state.getResultRegister
    var newState = condition.compile(state)

    /* Get the label IDs */
    val falseID = newState.nextID
    val continueID = newState.nextID

    /* Compare the condition with false (i.e. 0), mark the condition register as available to use and if condition is
       false, go to the false branch  */
    instructions ++= List(COMPARE(conditionReg, ImmediateNumber(0)), BRANCH(Option(EQ), labelPrefix + falseID))
    newState = newState.copy(freeRegs = conditionReg :: newState.freeRegs)

    /* Compile the true statement with a new scope */
    newState = trueStatement.compileNewScope(newState)
    /* We finished executing the branch - jump to the end. Compile the false branch with a new scope */
    instructions ++= List(BRANCH(None, labelPrefix + continueID), NumberLabel(falseID))
    newState = falseStatement.compileNewScope(newState)

    /* End of the if */
    instructions += NumberLabel(continueID)
    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (condition.getType(symbolTable).unifies(BooleanType())) {
      condition.check(symbolTable)

      val trueSymbolTable = new SymbolTable(symbolTable)
      trueStatement.check(trueSymbolTable)

      val falseSymbolTable = new SymbolTable(symbolTable)
      falseStatement.check(falseSymbolTable)
    } else {
      errors += Error(
        "If condition does not evaluate to Boolean. Got " + condition.getType(symbolTable) +
          " in " + condition.toString,
        getPos()
      )
    }
  }

  override def getPos(): (Int, Int) = position
}

case class Print(expression: Expression, isNewLine: Boolean)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression */
    val resultReg = state.getResultRegister
    var newState = expression.compile(state)

    /* Find the message format based on the expression's type */
    val format = (expression.getExpressionType match {
      case IntType()                                                 => "%d"
      case CharacterType()                                           => "%c"
      case StringType() | BooleanType() | ArrayType(CharacterType()) => "%s"
      case _                                                         => "%p"
    }) + (if (isNewLine) "\\n" else "")

    /* Get the format ID from the state */
    newState = newState.putMessageIfAbsent(format)
    val formatID = newState.getMessageID(format)

    /* printf first argument, the format; then printf second argument, the thing to be printed. */
    instructions ++= List(
      LOAD(Register0, MessageLoad(formatID)),
      ADD(Register0, Register0, ImmediateNumber(4)),
      MOVE(Register1, resultReg)
    )

    /* If a boolean, replace it with true or false */
    if (expression.getExpressionType == BooleanType()) {
      /* True and false IDs */
      newState = newState.putMessageIfAbsent("true")
      newState = newState.putMessageIfAbsent("false")
      val trueID = newState.getMessageID("true")
      val falseID = newState.getMessageID("false")

      instructions ++= List(
        COMPARE(Register1, ImmediateNumber(0)),
        LOAD(Register1, MessageLoad(falseID), cond = Some(EQ)),
        LOAD(Register1, MessageLoad(trueID), cond = Some(NE))
      )
    }

    expression.getExpressionType match {
      case StringType() | BooleanType() | ArrayType(CharacterType()) =>
        instructions += ADD(Register1, Register1, ImmediateNumber(4))
      case _ => ()
    }

    /* Call printf */
    instructions += BRANCHLINK("printf")

    /* Mark the result register as usable */
    newState.copy(freeRegs = resultReg :: newState.freeRegs)
  }

  override def toString: String = "print" + (if (isNewLine) "ln " else " ") + expression.toString + "\n"
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit =
    expression.check(symbolTable)
  override def getPos(): (Int, Int) = position
}

/* A read statement ‘read’ is a special assignment statement that takes its value from the standard input and writes it
   to its argument. Just like a general assignment statement, a read statement can target a program variable, an array
   element or a pair element. However, the read statement can only handle character or integer input. The read statement
   determines how it will interpret the value from the standard input based on the type of the target. For example, if
   the target is a variable of type ‘int’ then it will convert the input string into an integer. */
case class Read(assignmentLeft: AssignmentLeft)(position: (Int, Int)) extends Statement {
  override def toString: String = "read " + assignmentLeft.toString + "\n"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Find the reference to what we want to read and put in in r1 */
    val pointerReg = state.getResultRegister
    var newState = assignmentLeft.compileReference(state)
    instructions += MOVE(Register1, pointerReg)

    /* Decide if we read an int or a char */
    val format = assignmentLeft.getLeftType match {
      case IntType()       => " %d"
      case CharacterType() => " %c"
    }

    /* Get the format string ID */
    newState = newState.putMessageIfAbsent(format)
    val formatID = newState.getMessageID(format)

    /* Put the format in register 0 and call scanf */
    instructions ++= List(
      LOAD(Register0, MessageLoad(formatID)),
      ADD(Register0, Register0, ImmediateNumber(4)),
      BRANCHLINK("scanf")
    )

    /* Mark the pointer register as free */
    newState.copy(freeRegs = pointerReg :: newState.freeRegs)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    val assignmentLeftType = assignmentLeft.getType(symbolTable)
    if (
      assignmentLeftType.unifies(CharacterType()) ||
      assignmentLeftType.unifies(IntType())
    ) {
      assignmentLeft.check(symbolTable)
    } else {
      errors += Error("Read statement can only target characters and integers", getPos())
    }
  }

  override def getPos(): (Int, Int) = position
}

case class Return(expression: Expression)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression and store it in r0 */
    val resultReg = state.getResultRegister
    val newState = expression.compile(state)
    val functionDeclaredSize = newState.spOffset - newState.getOffset(Function.initSP)

    /* Return */
    instructions ++= List(
      MOVE(Register0, resultReg),
      ADD(RegisterSP, RegisterSP, ImmediateNumber(functionDeclaredSize)),
      PopPC()
    )

    /* Mark the result register as usable */
    newState.copy(spOffset = newState.getOffset(Function.initSP), freeRegs = resultReg :: newState.freeRegs)
  }
  override def toString: String = "return " + expression.toString + "\n"

  /* Ensure that Return call is inside a function and not global */
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (!symbolTable.isInsideFunctionSymbolTable()) {
      errors += Error("Return called in global scope - must be called within a function", getPos())
      return
    }
    val expectedReturnType = symbolTable.getReturnType()
    val actualReturnType = expression.getType(symbolTable)
    if (!expectedReturnType.unifies(actualReturnType)) {
      errors += Error(
        "Return type mismatch. Expected: " + expectedReturnType + ", Actual: " + actualReturnType,
        getPos()
      )
    }
    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

case class StatementFunctionCall(fcall: FunctionCall)(position: (Int, Int)) extends Statement {
  override def toString: String = fcall.toString

  override def check(symbolTable: SymbolTable)(implicit errors: ListBuffer[Error]): Unit =
    fcall.check(symbolTable)

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val resultReg = state.getResultRegister
    val newState = fcall.compile(state)
    newState.copy(freeRegs = resultReg :: newState.freeRegs)
  }
}

case class SkipStatement()(position: (Int, Int)) extends Statement {
  override def toString: String = "skip\n"
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = state
  override def getPos(): (Int, Int) = position
}

case class StatementSequence(statement1: Statement, statement2: Statement)(position: (Int, Int)) extends Statement {
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val nextState = statement1.compile(state)
    statement2.compile(nextState)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    statement1.check(symbolTable)
    statement2.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

case class While(condition: Expression, statement: Statement)(position: (Int, Int)) extends Statement {
  override def toString: String = "while " + condition.toString + " do\n" + statement.toString + "done\n"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val labelPrefix = "L"

    /* Get the label IDs */
    val conditionID = state.nextID
    val bodyID = state.nextID

    /* Branch to the condition and compile the while body with a new scope */
    instructions ++= List(BRANCH(None, labelPrefix + conditionID), NumberLabel(bodyID))
    var newState = statement.compileNewScope(state)

    /* Compile the condition */
    val conditionReg = newState.getResultRegister
    instructions += NumberLabel(conditionID)
    newState = condition.compile(newState)

    /* Check if the condition is true */
    instructions += COMPARE(conditionReg, ImmediateNumber(1))

    /* Mark the condition register as free to use */
    newState = newState.copy(freeRegs = conditionReg :: newState.freeRegs)

    /* Jump to the body if it is true */
    instructions += BRANCH(Option(EQ), labelPrefix + bodyID)
    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    val conditionType = condition.getType(symbolTable)
    if (conditionType.unifies(BooleanType())) {
      condition.check(symbolTable)
      val whileSymbolTable = new SymbolTable(symbolTable)
      statement.check(whileSymbolTable)
    } else {
      errors += Error(
        "While condition does not evaluate to boolean. Got type: " + conditionType.toString +
          ", in expression: " + condition.toString,
        position
      )
    }
  }

  override def getPos(): (Int, Int) = position
}

object Statement {
  def apply(action: Parsley[String], expr: Parsley[Expression]): Parsley[Statement] =
    pos <**> (action, expr).map {
      case ("free", e)    => Free(e)
      case ("return", e)  => Return(e)
      case ("exit", e)    => Exit(e)
      case ("print", e)   => Print(e, isNewLine = false)
      case ("println", e) => Print(e, isNewLine = true)
    }
}

object Assignment {
  def apply(left: Parsley[AssignmentLeft], right: Parsley[AssignmentRight]): Parsley[Assignment] =
    pos <**> (left, right).map(Assignment(_, _))
}

object BeginEnd {
  def apply(statement: Parsley[Statement]): Parsley[BeginEnd] =
    pos <**> statement.map(BeginEnd(_))
}

object IdentifierDeclaration {
  def apply(
    identType: Parsley[Type],
    name: Parsley[Identifier],
    value: Parsley[AssignmentRight]
  ): Parsley[IdentifierDeclaration] = pos <**> (identType, name, value).map(IdentifierDeclaration(_, _, _))
}

object If {
  def apply(
    cond: Parsley[Expression],
    trueStatement: Parsley[Statement],
    falseStatement: Parsley[Statement]
  ): Parsley[If] = pos <**> (cond, trueStatement, falseStatement).map(If(_, _, _))
}

object Read {
  def apply(left: Parsley[AssignmentLeft]): Parsley[Read] = pos <**> left.map(Read(_))
}

object SkipStatement {
  def apply(skip: Parsley[String]): Parsley[SkipStatement] = pos <**> skip.map(_ => SkipStatement())
}

object StatementSequence {
  def apply(statementLeft: Parsley[Statement], statementRight: Parsley[Statement]): Parsley[StatementSequence] =
    pos <**> (statementLeft, statementRight).map(StatementSequence(_, _))
}

object While {
  def apply(cond: Parsley[Expression], body: Parsley[Statement]): Parsley[While] =
    pos <**> (cond, body).map(While(_, _))
}

object StatementFunctionCall {
  def apply(functionCall: Parsley[FunctionCall]): Parsley[StatementFunctionCall] =
    pos <**> functionCall.map(StatementFunctionCall(_))
}