package com.wacc

case class Program(functions: List[Function], body: Statement)

case class Function(
    functionType: Type,
    functionName: Identifier,
    parameters: Option[ParameterList],
    body: Statement
)

case class ParameterList(parameters: List[Parameter])

case class Parameter(parameterType: Type, identifier: Identifier)
