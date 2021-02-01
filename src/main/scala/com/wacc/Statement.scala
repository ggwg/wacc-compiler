package com.wacc

sealed trait Statement

case class Assignment(
    assignmentLeft: AssignmentLeft,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    assignmentLeft.toString + " = " + assignmentRight.toString + "\n"
}

case class BeginEnd(statement: Statement) extends Statement {
  override def toString: String = "begin\n" + statement.toString + "end\n"
}

case class Exit(expression: Expression) extends Statement {
  override def toString: String = "exit " + expression.toString + "\n"
}

case class Free(expression: Expression) extends Statement {
  override def toString: String = "free " + expression.toString + "\n"
}

case class IdentifierDeclaration(
    identType: Type,
    identifier: Identifier,
    assignmentRight: AssignmentRight
) extends Statement {
  override def toString: String =
    identType.toString + " " + identifier.toString + " = " + assignmentRight.toString + "\n"
}

case class If(
    condition: Expression,
    trueStatement: Statement,
    falseStatement: Statement
) extends Statement {
  override def toString: String =
    "if " + condition + " then\n" + trueStatement.toString + "else\n" + falseStatement + "fi\n"
}

case class Print(expression: Expression) extends Statement {
  override def toString: String = "print " + expression.toString + "\n"
}

case class Println(expression: Expression) extends Statement {
  override def toString: String = "println " + expression.toString + "\n"
}

case class Read(assignmentLeft: AssignmentLeft) extends Statement {
  override def toString: String = "read " + assignmentLeft.toString + "\n"
}

case class Return(expression: Expression) extends Statement {
  override def toString: String = "return " + expression.toString + "\n"
}

case class SkipStatement() extends Statement {
  override def toString: String = "skip\n"
}

case class StatementSequence(
    statement1: Statement,
    statement2: Statement
) extends Statement {
  override def toString: String =
    statement1.toString.stripSuffix("\n") + ";\n" + statement2.toString
}

case class While(condition: Expression, statement: Statement)
    extends Statement {
  override def toString: String =
    "while " + condition.toString + " do\n" + statement.toString + "done\n"
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
