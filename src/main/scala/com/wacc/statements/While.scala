package com.wacc.statements

import com.wacc.expressions.Expression

class While(val condition: Expression, val statement: Statement)
    extends Statement {}
