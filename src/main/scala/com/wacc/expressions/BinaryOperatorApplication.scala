package com.wacc.expressions

import com.wacc.binaryoperators.BinaryOperator

class BinaryOperatorApplication(
    val expression1: Expression,
    val binaryOperator: BinaryOperator,
    val expression2: Expression
) extends Expression {}
