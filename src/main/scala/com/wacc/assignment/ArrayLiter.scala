package com.wacc.assignment

import com.wacc.expressions.Expression

class ArrayLiter(val expressions: List[Expression])
    extends Expression
    with AssignmentRight {}
