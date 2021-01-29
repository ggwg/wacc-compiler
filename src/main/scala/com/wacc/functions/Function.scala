package com.wacc.functions

import com.wacc.expressions.Identifier
import com.wacc.statements.Statement
import com.wacc.types.Type

class Function(
    val functionType: Type,
    val functionName: Identifier,
    val parameters: Option[ParameterList],
    val body: Statement
) {}
