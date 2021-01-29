package com.wacc.expressions

import com.wacc.unaryoperators.UnaryOperator

class UnaryOperatorApplication(
    val unaryOperator: UnaryOperator,
    val expression: Expression
) extends Expression {}
