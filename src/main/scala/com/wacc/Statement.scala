package com.wacc

sealed trait Statement

case class Assignment(
    assignmentLeft: AssignmentLeft,
    assignmentRight: AssignmentRight
) extends Statement

case class BeginEnd(val statement: Statement) extends Statement

case class Exit(expression: Expression) extends Statement

case class Free(expression: Expression) extends Statement

case class IdentifierDeclaration(
    val identType: Type,
    val identifier: Identifier,
    val assignmentRight: AssignmentRight
) extends Statement

case class If(
    val condition: Expression,
    val trueStatement: Statement,
    val falseStatement: Statement
) extends Statement

case class Print(val expression: Expression) extends Statement

case class Println(val expression: Expression) extends Statement

case class Read(assignmentLeft: AssignmentLeft) extends Statement

case class Return(expression: Expression) extends Statement

case class SkipStatement() extends Statement

case class StatementSequence(
    val statement1: Statement,
    val statement2: Statement
) extends Statement

case class While(val condition: Expression, val statement: Statement)
    extends Statement
