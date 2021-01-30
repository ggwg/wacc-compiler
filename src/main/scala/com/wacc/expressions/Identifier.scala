package com.wacc.expressions

import com.wacc.assignment.AssignmentLeft

class Identifier(val identifier: String)
    extends Expression
    with AssignmentLeft {}
