package com.wacc

import parsley.Parsley
import parsley.Parsley.{pos, select}
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
    /* Create a new state for the new scope */
    var scopeState = state.newScopeState

    /* Compile the body with the new scope */
    scopeState = this.compile(scopeState)

    /* Reset the SP to where we were initially */
    instructions += ADD(RegisterSP, RegisterSP, ImmediateNumber(scopeState.declaredSize))

    /* Restore the state to hold the correct fields */
    scopeState.fromScopeToInitialState(state)
  }
}

/* ✅ Check done - ⚠️ Compile done */
case class IdentifierDeclaration(identType: Type, name: Identifier, assignmentRight: AssignmentRight)(
  position: (Int, Int)
) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the right hand side */
    val resultReg = state.getResultRegister
    var newState = assignmentRight.compile(state)

    /* Store the identifier on the stack */
    val size = identType.getSize
    instructions += SUB(RegisterSP, RegisterSP, ImmediateNumber(size))
    instructions += STORE(resultReg, RegisterLoad(RegisterSP), size == 1)

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
        // If left type == right type, then we can add it to dictionary.
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
    })
  }

  override def getPos(): (Int, Int) = position
}

/* ✅ Check done - ⚠️ Compile done */
case class Assignment(assignmentLeft: AssignmentLeft, assignmentRight: AssignmentRight)(position: (Int, Int))
    extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the pointer of the thing that will be assigned */
    val assignmentRegister = state.getResultRegister
    var newState = assignmentRight.compile(state)

    /* Compile the value to be assigned to the left side */
    val assignPointer = newState.getResultRegister
    newState = assignmentLeft.compileReference(newState)

    /* Assign the value */
    instructions += STORE(assignmentRegister, RegisterLoad(assignPointer), assignmentLeft.getLeftType.getSize == 1)

    /* Mark the registers as being usable */
    newState.copy(freeRegs = assignmentRegister :: assignPointer :: newState.freeRegs)
  }
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Check that assignment-left type is same as return type of assignment-right */
    assignmentLeft match {
      case Identifier(identifier) =>
        if (symbolTable.identifierIsFunction(identifier)) {
          errors += Error("Function identifier " + identifier + " cannot be assigned", getPos())
          return
        }
      case _ => ()
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

/* ✅ Check done - ✅ Compile done */
case class BeginEnd(statement: Statement)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    statement.compileNewScope(state)
  }

  override def toString: String = "begin\n" + statement.toString + "end\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    // Create new scope for Symbol Table
    val beginEndSymbolTable = new SymbolTable(symbolTable)

    // Recursively call check.
    statement.check(beginEndSymbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* ✅ Check done - ⚠️ Compile Pending */
case class Exit(expression: Expression)(position: (Int, Int)) extends Statement {

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {

    /* Compile the expression and store it in r0 */
    val resultReg = state.getResultRegister
    val newState = expression.compile(state)
    instructions += MOVE(Register0, resultReg)

    /* Call the exit function */
    instructions += BRANCHLINK("exit")

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
   memory that stores the pair/array itself is also freed.
   ✅ Check done - ⚠️ Compile Pending */
case class Free(expression: Expression)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression */
    val resultReg = state.getResultRegister
    var newState = expression.compile(state)
    /* TODO: Null check */

    expression.getExpressionType match {
      case ArrayType(_) =>
        /* Free the array memory */
        instructions += MOVE(Register0, resultReg)
        instructions += BRANCHLINK("free")
      case PairType(_, _) =>
        instructions += MOVE(Register0, resultReg)

        /* Save the pair pointer on the stack */
        instructions += PUSH(Register0)

        /* Free the first pointer */
        instructions += LOAD(Register0, RegisterLoad(Register0))
        instructions += BRANCHLINK("free")

        /* Retrieve the pair pointer */
        instructions += LOAD(Register0, RegisterLoad(RegisterSP))

        /* Free the second pointer */
        instructions += LOAD(Register0, RegisterOffsetLoad(Register0, ImmediateNumber(4)))
        instructions += BRANCHLINK("free")

        /* Free the pair pointer */
        instructions += POP(Register0)
        instructions += BRANCHLINK("free")
    }
    newState.copy(freeRegs = resultReg :: newState.freeRegs)
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

/* ✅ Check done - ✅️ Compile Done */
case class If(condition: Expression, trueStatement: Statement, falseStatement: Statement)(position: (Int, Int))
    extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {

    /* Compile the if condition */
    val conditionReg = state.getResultRegister
    var newState = condition.compile(state)

    /* Get the label IDs */
    val falseID = newState.nextID
    val continueID = newState.nextID

    /* Compare the condition with false (i.e. 0) */
    instructions += COMPARE(conditionReg, ImmediateNumber(0))

    /* Mark the condition register as available to use */
    newState = newState.copy(freeRegs = conditionReg :: newState.freeRegs)

    /* If condition is false, go to the false branch */
    instructions += BRANCH(Option(EQ), "L" + falseID)

    /* Compile the true statement with a new scope */
    newState = trueStatement.compileNewScope(newState)

    /* We finished executing the branch. Jump to the end. */
    instructions += BRANCH(None, "L" + continueID)

    /* Compile the false branch with a new scope */
    instructions += NumberLabel(falseID)
    newState = falseStatement.compileNewScope(newState)

    /* End of the if */
    instructions += NumberLabel(continueID)
    newState
  }
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"

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

/* ✅ Check done - ⚠️ Compile done */
case class Print(expression: Expression, isNewLine: Boolean)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the expression */
    val resultReg = state.getResultRegister
    var newState = expression.compile(state)

    /* Find the message format based on the expression's type */
    val format = (expression.getExpressionType match {
      case IntType() =>
        "%d"
      case CharacterType() =>
        "%c"
      case StringType() | BooleanType() | ArrayType(CharacterType()) =>
        "%s"
      case _ =>
        "%p"
    }) + (if (isNewLine) "\\n" else "")

    /* Get the format ID from the state */
    newState = newState.putMessageIfAbsent(format)
    val formatID = newState.getMessageID(format)

    /* printf first argument, the format */
    instructions += LOAD(Register0, MessageLoad(formatID))
    instructions += ADD(Register0, Register0, ImmediateNumber(4))

    /* printf second argument, the thing to be printed */
    instructions += MOVE(Register1, resultReg)

    /* If a boolean, replace it with true or false */
    if (expression.getExpressionType == BooleanType()) {

      /* True and false IDs */
      newState = newState.putMessageIfAbsent("true")
      newState = newState.putMessageIfAbsent("false")
      val trueID = newState.getMessageID("true")
      val falseID = newState.getMessageID("false")

      instructions += COMPARE(Register1, ImmediateNumber(0))
      instructions += LOAD(Register1, MessageLoad(falseID), cond = Some(EQ))
      instructions += LOAD(Register1, MessageLoad(trueID), cond = Some(NE))
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

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* A read statement ‘read’ is a special assignment statement that takes its value from the standard input and writes it
   to its argument. Just like a general assignment statement, a read statement can target a program variable, an array
   element or a pair element. However, the read statement can only handle character or integer input. The read statement
   determines how it will interpret the value from the standard input based on the type of the target. For example, if
   the target is a variable of type ‘int’ then it will convert the input string into an integer.
   ✅ Check done - ⚠️ Compile Pending
 */
case class Read(assignmentLeft: AssignmentLeft)(position: (Int, Int)) extends Statement {
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

    /* Put the format in register 0 */
    instructions += LOAD(Register0, MessageLoad(formatID))
    instructions += ADD(Register0, Register0, ImmediateNumber(4))

    /* Call scanf */
    instructions += BRANCHLINK("scanf")

    /* Mark the pointer register as free */
    newState.copy(freeRegs = pointerReg :: newState.freeRegs)
  }

  override def toString: String = "read " + assignmentLeft.toString + "\n"

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

/* ✅ Check done - ⚠️ Compile done */
case class Return(expression: Expression)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {

    /* Compile the expression and store it in r0 */
    val resultReg = state.getResultRegister
    val newState = expression.compile(state)
    val functionDeclaredSize = newState.spOffset - newState.getOffset(Function.initSP)
    instructions += MOVE(Register0, resultReg)

    /* Return */
    instructions += ADD(RegisterSP, RegisterSP, ImmediateNumber(functionDeclaredSize))
    instructions += PopPC()

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

/* ✅ Check done - ✅ Compile done */
case class SkipStatement()(position: (Int, Int)) extends Statement {

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    state
  }

  override def toString: String = "skip\n"

  override def getPos(): (Int, Int) = position
}

/* ✅ Check done - ✅ Compile done */
case class StatementSequence(statement1: Statement, statement2: Statement)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val nextState = statement1.compile(state)
    statement2.compile(nextState)
  }
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    statement1.check(symbolTable)
    statement2.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* ✅ Check done - ✅ Compile done */
case class While(condition: Expression, statement: Statement)(position: (Int, Int)) extends Statement {
  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Get the label IDs */
    val conditionID = state.nextID
    val bodyID = state.nextID

    /* Branch to the condition */
    instructions += BRANCH(None, "L" + conditionID)

    /* Compile the while body with a new scope */
    instructions += NumberLabel(bodyID)
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
    instructions += BRANCH(Option(EQ), "L" + bodyID)

    newState
  }

  override def toString: String =
    "while " + condition.toString + " do\n" + statement.toString + "done\n"

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
