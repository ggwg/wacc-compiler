package com.wacc.statements

import com.wacc.assignment.AssignmentRight
import com.wacc.expressions.Identifier
import com.wacc.types.Type

class IdentifierDeclaration(
    val identType: Type,
    val identifier: Identifier,
    val assignmentRight: AssignmentRight
) extends Statement {}
