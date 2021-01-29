package com.wacc.assignment

import com.wacc.expressions.Identifier

class FunctionCall(identifier: Identifier, arguments: Option[ArgumentList])
    extends AssignmentRight {}
