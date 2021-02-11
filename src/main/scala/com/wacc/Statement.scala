package com.wacc

import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable

sealed trait Statement extends ASTNodeVoid {
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
}

/* Check Done */
case class Assignment(assignmentLeft: AssignmentLeft, assignmentRight: AssignmentRight)(position: (Int, Int))
    extends Statement {
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE ASSIGNMENT CHECK")
    var pos = getPos()
    /* Check that assignment-left type is same as return type of assignment-right */
    if (
      assignmentLeft
        .getType(symbolTable)
        .unifies(assignmentRight.getType(symbolTable))
    ) {
      assignmentLeft.check(symbolTable)
      assignmentRight.check(symbolTable)
    } else {
      errors += DefaultError("Type mismatch in Assigment for " + this.toString, pos)
    }
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class BeginEnd(statement: Statement)(position: (Int, Int)) extends Statement {
  override def toString: String = "begin\n" + statement.toString + "end\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE BEGIN-END CHECK")
    // Create new scope for Symbol Table
    var beginEndSymbolTable = new SymbolTable(symbolTable)
    // Recursively call check.
    statement.check(beginEndSymbolTable)
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class Exit(expression: Expression)(position: (Int, Int)) extends Statement {
  override def toString: String = "exit " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE EXIT CHECK")
    var pos = getPos()
    if (!expression.getType(symbolTable).unifies(IntType())) {
      errors += DefaultError("Exit expression not type Int", pos)
    }
  }

  override def getPos(): (Int, Int) = position
}

/*
 * Memory Free Statements:
A memory free statement ‘free’ is used to free the heap memory allo-
cated for a pair or array and its immediate content. The statement is given an expression that must be
of type ‘pair(T1, T2)’ or ‘T[]’ (for some T, T1, T2). The expression must evaluate to a valid reference
to a pair or array, otherwise a segmentation fault will occur at runtime.
If the reference is valid, then the memory for each element of the pair/array is freed, so long as the
element is not a reference to another pair or another array (i.e. free is not recursive). Then the memory
that stores the pair/array itself is also freed.
 * */
case class Free(expression: Expression)(position: (Int, Int)) extends Statement {
  override def toString: String = "free " + expression.toString + "\n"

  // TODO: this
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE FREE CHECK")
    var pos = getPos()
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class IdentifierDeclaration(identType: Type, identifier: Identifier, assignmentRight: AssignmentRight)(
  position: (Int, Int)
) extends Statement {
  override def toString: String =
    identType.toString + " " + identifier.toString + " = " + assignmentRight.toString + "\n"
  /* Check if identifier is already defined in the symbol table (current, not parent)
   * If so, then record error because variable names must not class with existing
   * variable names or any keyword. Extract type of identType, then we check if this
   * is the same type as assignmentRight. If so, add it to symbol table. Else, error.
   */
  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE IDENTIFIER-DECLARATION CHECK")
    var pos = getPos()
    symbolTable.dictionary.updateWith(identifier.identifier)({
      case Some(x) => {
        errors +=
          DefaultError("Variable declaration " + identifier.identifier + " already defined in current scope.", pos)
        Some(x)
      }
      case None => {
        // If left type == right type, then we can add it to dictionary.
        if (identType.sameTypes(assignmentRight, symbolTable)) {
          Some((identType.getType(symbolTable), assignmentRight))
        } else {
          errors += DefaultError(
            "Invalid types in identifier assignment. Got: " +
              identType.getType(symbolTable) + assignmentRight.getType(symbolTable),
            pos
          )
          None
        }
      }
    })
  }

  override def getPos(): (Int, Int) = position
}

/* Check done - Do we have to do the check for the true and false branches even if the
 * provided condition is not of the right type? */
case class If(condition: Expression, trueStatement: Statement, falseStatement: Statement)(position: (Int, Int))
    extends Statement {
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE IF CHECK")
    var pos = getPos()
    if (condition.getType(symbolTable).unifies(BooleanType())) {
      condition.check(symbolTable)
      var trueSymbolTable = new SymbolTable(symbolTable)
      trueStatement.check(trueSymbolTable)
      var falseSymbolTable = new SymbolTable(symbolTable)
      falseStatement.check(falseSymbolTable)
    } else {
      errors += DefaultError(
        "If condition does not evaluate to Boolean. Got " + condition.getType(symbolTable) +
          " in " + condition.toString,
        pos
      )
    }
    List.empty
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class Print(expression: Expression)(position: (Int, Int)) extends Statement {
  override def toString: String = "print " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE PRINT CHECK")
    if (expression.getType(symbolTable).unifies(StringType())) {
      expression.check(symbolTable)
    } else {
      println(
        "Error -- Print statement expects String, but found " +
          expression.getClass.toString
      )
    }
    List.empty
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class Println(expression: Expression)(position: (Int, Int)) extends Statement {
  override def toString: String = "println " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE PRINTLN CHECK")
    if (expression.getType(symbolTable).unifies(StringType())) {
      expression.check(symbolTable)
    } else {
      println(
        "Error -- Println statement expects String, but found " +
          expression.getClass.toString
      )
    }
    List.empty
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class Read(assignmentLeft: AssignmentLeft)(position: (Int, Int)) extends Statement {
  override def toString: String = "read " + assignmentLeft.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE READ CHECK")
    val assignmentLeftType = assignmentLeft.getType(symbolTable)
    if (
      assignmentLeftType.unifies(CharacterType()) ||
      assignmentLeftType.unifies(IntType())
    ) {
      assignmentLeft.check(symbolTable)
    } else {
      println(
        "Error -- Read expects Char or Int, but found " +
          assignmentLeft.getClass.toString
      )
    }
    List.empty
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class Return(expression: Expression)(position: (Int, Int)) extends Statement {
  override def toString: String = "return " + expression.toString + "\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE RETURN CHECK")
    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type =
    expression.getType(symbolTable)
}

/* Check done */
case class SkipStatement()(position: (Int, Int)) extends Statement {
  override def toString: String = "skip\n"

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class StatementSequence(statement1: Statement, statement2: Statement)(position: (Int, Int)) extends Statement {
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE STATEMENT-SEQUENCE CHECK")
    statement1.check(symbolTable)
    statement2.check(symbolTable)
    List.empty
  }

  override def getPos(): (Int, Int) = position
}

/* Check done */
case class While(condition: Expression, statement: Statement)(position: (Int, Int)) extends Statement {
  override def toString: String =
    "while " + condition.toString + " do\n" + statement.toString + "done\n"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println("GOT INSIDE WHILE CHECK")
    if (condition.getType(symbolTable).unifies(BooleanType())) {
      condition.check(symbolTable)
      var whileSymbolTable = new SymbolTable(symbolTable)
      statement.check(whileSymbolTable)
    } else {
      println(
        "Error -- Condition of if statement expects a boolean, but found " +
          condition.getClass.toString
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
      case ("print", e)   => Print(e)
      case ("println", e) => Println(e)
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
