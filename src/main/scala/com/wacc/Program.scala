package com.wacc

case class Program(val functions: List[Function], val body: Statement)

case class Function(
    val functionType: Type,
    val functionName: Identifier,
    val parameters: Option[ParameterList],
    val body: Statement
)

case class ParameterList(val parameters: List[Parameter])

case class Parameter(val parameterType: Type, val identifier: Identifier)
