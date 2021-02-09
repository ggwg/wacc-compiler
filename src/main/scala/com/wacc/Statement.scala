package com.wacc

sealed trait Statement extends ASTNodeVoid

/* Check Done */
case class Assignment(
    assignmentLeft: AssignmentLeft,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ASSIGNMENT CHECK")
    
    /* Check that assignment-left type is same as return type of assignment-right */
    println(assignmentLeft.getType(symbolTable))
    println(assignmentRight.getType(symbolTable))
    if (assignmentLeft.getType(symbolTable).unifies(assignmentRight.getType(symbolTable))) {
      assignmentLeft.check(symbolTable)
      assignmentRight.check(symbolTable)
    } else {
      println("Left assignment type does not match right assigment type")
    }
  }
}

/* Check done */
case class BeginEnd(statement: Statement) extends Statement {
  override def toString: String = "begin\n" + statement.toString + "end\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BEGIN-END CHECK")
    // Create new scope for Symbol Table
    var newSymbolTable = new SymbolTable(symbolTable)
    // Recursively call check.
    statement.check(newSymbolTable)
  }
}

/* Check done */
case class Exit(expression: Expression) extends Statement {
  override def toString: String = "exit " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE EXIT CHECK")
    if (expression.getType(symbolTable).unifies(IntType())) {
      println("Success")
    } else {
      println("Error: Exit type invalid -- expected IntType, but got"
        + expression.getClass.toString)
    }
    return ()
  }
}

/* Check done - Though, should the expression type be string? */
case class Free(expression: Expression) extends Statement {
  override def toString: String = "free " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE FREE CHECK")
    if (expression.getType(symbolTable).unifies(StringType())) {
      println("Success")
    } else {
      println("Error: Type invalid -- expected StringType, but got"
        + expression.getClass.toString)
    }
    return ()
  }
}

/* Check done */
case class IdentifierDeclaration(
    identType: Type,
    identifier: Identifier,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    identType.toString + " " + identifier.toString + " = " + assignmentRight.toString + "\n"
  /* Check if identifier is already defined in the symbol table (current, not parent)
   * If so, then record error because variable names must not class with existing
   * variable names or any keyword. Extract type of identType, then we check if this
   * is the same type as assignmentRight. If so, add it to symbol table. Else, error.
   */
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IDENTIFIER-DECLARATION CHECK")
    symbolTable.dictionary.updateWith(identifier.identifier)({
      case Some(x) => {
        println("Already in Dictionary - throw error")
        Some(x)
      }
      case None => {
        if (identType.getType(symbolTable).unifies(assignmentRight.getType(symbolTable))) {
          println("Added to dictionary")
          Some((identType.getType(symbolTable), assignmentRight))
        } else {
          println("Invalid Types!")
          None
        }
      }
    })
  }
}

/* Check done - Do we have to do the check for the true and false branches even if the
 * provided condition is not of the right type? */
case class If(
    condition: Expression,
    trueStatement: Statement,
    falseStatement: Statement
) extends Statement {
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IF CHECK")
    if (condition.getType(symbolTable).unifies(BooleanType())) {
      var trueSymbolTable = new SymbolTable(symbolTable)
      trueStatement.check(trueSymbolTable)
      var falseSymbolTable = new SymbolTable(symbolTable)
      falseStatement.check(falseSymbolTable)
    } else {
      println("Error -- Condition of if statment expects a boolean, but found " +
        condition.getClass.toString)
    }
  }
}

/* Check done */
case class Print(expression: Expression) extends Statement {
  override def toString: String = "print " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PRINT CHECK")
    if (expression.getType(symbolTable).unifies(StringType())) {
      expression.check(symbolTable)
    } else {
      println("Error -- Print statement expects String, but found " +
        expression.getClass.toString)
    }
  }
}

/* Check done */
case class Println(expression: Expression) extends Statement {
  override def toString: String = "println " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PRINTLN CHECK")
    if (expression.getType(symbolTable).unifies(StringType())) {
      expression.check(symbolTable)
    } else {
      println("Error -- Println statement expects String, but found " +
        expression.getClass.toString)
    }
  }
}

/* Check done */
case class Read(assignmentLeft: AssignmentLeft) extends Statement {
  override def toString: String = "read " + assignmentLeft.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE READ CHECK")
    val assignmentLeftType = assignmentLeft.getType(symbolTable)
    if (assignmentLeftType.unifies(CharacterType()) ||
      assignmentLeftType.unifies(IntType())) {
      assignmentLeft.check(symbolTable)
    } else {
      println("Error -- Read expects Char or Int, but found " +
        assignmentLeft.getClass.toString)
    }
  }
}

/* Check done */
case class Return(expression: Expression) extends Statement {
  override def toString: String = "return " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE RETURN CHECK")
    expression.check(symbolTable)
  }

  override def getType(symbolTable: SymbolTable): Type = expression.getType(symbolTable)
}

/* Check done */
case class SkipStatement() extends Statement {
  override def toString: String = "skip\n"
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE SKIP-STATEMENT CHECK")
  }
}

/* Check done */
case class StatementSequence(
    statement1: Statement,
    statement2: Statement
) extends Statement {
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE STATEMENT-SEQUENCE CHECK")
    statement1.check(symbolTable)
    statement2.check(symbolTable)
  }
}

/* Check done */
case class While(condition: Expression, statement: Statement)
    extends Statement {
  override def toString: String =
    "while " + condition.toString + " do\n" + statement.toString + "done\n"

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE WHILE CHECK")
    if (condition.getType(symbolTable).unifies(BooleanType())) {
      condition.check(symbolTable)
      var whileSymbolTable = new SymbolTable(symbolTable)
      statement.check(whileSymbolTable)
    } else {
      println("Error -- Condition of if statement expects a boolean, but found " +
        condition.getClass.toString)
    }
  }
}

object Statement {
  val buildActionExpression: (String, Expression) => Statement = {
    case ("free", e)    => Free(e)
    case ("return", e)  => Return(e)
    case ("exit", e)    => Exit(e)
    case ("print", e)   => Print(e)
    case ("println", e) => Println(e)
  }
}

object Assignment {
  val build: (AssignmentLeft, AssignmentRight) => Assignment = Assignment(_, _)
}

object BeginEnd {
  val build: Statement => BeginEnd = BeginEnd(_)
}

object Exit {
  val build: Expression => Exit = Exit(_)
}

object Free {
  val build: Expression => Free = Free(_)
}

object IdentifierDeclaration {
  val build: (Type, Identifier, AssignmentRight) => IdentifierDeclaration =
    IdentifierDeclaration(_, _, _)
}

object If {
  val build: (Expression, Statement, Statement) => If = If(_, _, _)
}

object Print {
  val build: Expression => Print = Print(_)
}

object Println {
  val build: Expression => Println = Println(_)
}

object Read {
  val build: AssignmentLeft => Read = Read(_)
}

object Return {
  val build: Expression => Return = Return(_)
}

object SkipStatement {
  val build: String => SkipStatement = _ => SkipStatement()
}

object StatementSequence {
  val build: (Statement, Statement) => StatementSequence =
    StatementSequence(_, _)
}

object While {
  val build: (Expression, Statement) => While = While(_, _)
}
