package com.wacc

sealed trait Statement extends ASTNode

case class Assignment(
    assignmentLeft: AssignmentLeft,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    // Check that assignment-left type is same as return type of assignment-right

    println("GOT INSIDE ASSIGNMENT CHECK")
    assignmentLeft.check(symbolTable)
    assignmentLeft.check(symbolTable)

    // (type1, errors: List[String]) = assignmentL.check(symbolTable)
    // type2 = assignmentR.check(symbolTable)
    // if type1 == type2:
    //   correct
    // else:
    //   return (Unit,

  }
}

case class BeginEnd(statement: Statement) extends Statement {
  override def toString: String = "begin\n" + statement.toString + "end\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BEGIN-END CHECK")
  }
}

case class Exit(expression: Expression) extends Statement {
  override def toString: String = "exit " + expression.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE EXIT CHECK")
    // Exit must return an integer literal error code.
    var exitType = expression.check(symbolTable)
    println(exitType + " - " + exitType.getClass)
    if (exitType.getClass == IntType.getClass) {
      // Correct
      println("SUCCESS")
    } else {
      println(
        "ERROR: Exit type invalid - expected " +
          IntType.getClass.toString + "Got: " + exitType.getClass.toString
      )
    }
    return ()
  }
}

case class Free(expression: Expression) extends Statement {
  override def toString: String = "free " + expression.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE FREE CHECK")
  }
}

case class IdentifierDeclaration(
    identType: Type,
    identifier: Identifier,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    identType.toString + " " + identifier.toString + " = " + assignmentRight.toString + "\n"

  /*
  Check if identifier is already defined in the symbol table (current, not parent)
    If so, then record error because variable names must not class with existing variable names
    or any keyword.
  Extract type of identType, then we check if this is the same type as assignmentRight.
    If so, add it to symbol table
    Else, error.
   */
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IDENTIFIER-DECLARATION CHECK")
    var optionTuple: Option[(Any, ASTNode)] = symbolTable.lookup(identifier.identifier)
    if (optionTuple.isEmpty) {
      // Don't throw an error - variable is not already defined in current scope
      println("Identifier not found in current Symbol Table")
      // Check identType is the same type as assignmentRight
      if (identType.getType(symbolTable) == assignmentRight.getType(symbolTable)) {
        // Add identifier
        symbolTable.add(
          identifier.identifier,
          identType.getClass,
          assignmentRight
        )
        println("Added to ST")
      } else {
        // Error - identType different type to assignmentRightType
        println("Error: identType different type to assignmentRightType")
        return ()
      }
    } else {
      // Error - variable already defined in current scope.
      print("Error - variable already defined in current scope")
      return ()
    }
//    identType.check(symbolTable)
//    identifier.check(symbolTable)
//    assignmentRight.check(symbolTable)
  }
}

case class If(
    condition: Expression,
    trueStatement: Statement,
    falseStatement: Statement
) extends Statement {
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IF CHECK")
    // Check the condition check it is a boolean type to a boolean
    val conditionType = condition.check(symbolTable)
    if (conditionType.getClass == BooleanType.getClass) {
      println("Bool type found for if cond")
      // Create new symbol table for checking types inside trueStatement
      var trueSymbolTable = new SymbolTable(symbolTable)
      trueStatement.check(trueSymbolTable)
      // Create new symbol table for checking types inside falseStatement
      var falseSymbolTable = new SymbolTable(symbolTable)
      falseStatement.check(falseSymbolTable)
    } else {
      println("Error: Bool type NOT found for if cond")
      return ()
    }
  }

}

case class Print(expression: Expression) extends Statement {
  override def toString: String = "print " + expression.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PRINT CHECK")
  }
}

case class Println(expression: Expression) extends Statement {
  override def toString: String = "println " + expression.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PRINTLN CHECK")
  }
}

case class Read(assignmentLeft: AssignmentLeft) extends Statement {
  override def toString: String = "read " + assignmentLeft.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE READ CHECK")
  }
}

case class Return(expression: Expression) extends Statement {
  override def toString: String = "return " + expression.toString + "\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE RETURN CHECK")
  }
}

case class SkipStatement() extends Statement {
  override def toString: String = "skip\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE SKIP-STATEMENT CHECK")
  }
}

case class StatementSequence(
    statement1: Statement,
    statement2: Statement
) extends Statement {
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE STATEMENT-SEQUENCE CHECK")
    statement1.check(symbolTable)
    statement2.check(symbolTable)
  }
}

case class While(condition: Expression, statement: Statement)
    extends Statement {
  override def toString: String =
    "while " + condition.toString + " do\n" + statement.toString + "done\n"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE WHILE CHECK")
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
