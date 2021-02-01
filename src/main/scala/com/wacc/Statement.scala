package com.wacc

sealed trait Statement

case class Assignment(
    assignmentLeft: AssignmentLeft,
    assignmentRight: AssignmentRight
) extends Statement

case class BeginEnd(statement: Statement) extends Statement

case class Exit(expression: Expression) extends Statement

case class Free(expression: Expression) extends Statement

case class IdentifierDeclaration(
    identType: Type,
    identifier: Identifier,
    assignmentRight: AssignmentRight
) extends Statement

case class If(
    condition: Expression,
    trueStatement: Statement,
    falseStatement: Statement
) extends Statement

case class Print(expression: Expression) extends Statement

case class Println(expression: Expression) extends Statement

case class Read(assignmentLeft: AssignmentLeft) extends Statement

case class Return(expression: Expression) extends Statement

case class SkipStatement() extends Statement

case class StatementSequence(
    statement1: Statement,
    statement2: Statement
) extends Statement

case class While(condition: Expression, statement: Statement) extends Statement
