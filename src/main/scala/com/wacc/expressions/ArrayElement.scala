package com.wacc.expressions

import com.wacc.assignment.AssignmentLeft

class ArrayElement(
    val identifier: Identifier,
    val expressions: List[Expression]
) extends Expression
    with AssignmentLeft {}
