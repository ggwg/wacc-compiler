package com.wacc.statements

import com.wacc.expressions.Expression

class If(
    val condition: Expression,
    val trueStatement: Statement,
    val falseStatement: Statement
) extends Statement {}
