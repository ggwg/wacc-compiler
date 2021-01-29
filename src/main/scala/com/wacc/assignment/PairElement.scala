package com.wacc.assignment

import com.wacc.expressions.Expression

class PairElement(val firstField: Boolean, val expression: Expression)
    extends AssignmentRight
    with AssignmentLeft {}
