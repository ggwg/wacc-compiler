package com.wacc.assignment

import com.wacc.expressions.Expression

class PairElement(val firstField: Expression, val secondField: Expression)
    extends AssignmentRight
    with AssignmentLeft {}
