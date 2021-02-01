package com.wacc

case class Program(functions: List[Function], body: Statement) {
  override def toString: String = "begin\n" + functions
    .map(_.toString)
    .reduce((left, right) => left + right) + body.toString + "end"
}

case class Function(
    functionType: Type,
    functionName: Identifier,
    parameters: Option[ParameterList],
    body: Statement
) {
  override def toString: String =
    functionType.toString + " " + functionName + "(" + parameters
      .getOrElse("")
      .toString + ")" + "is\n" + body.toString + "end\n"
}

case class ParameterList(parameters: List[Parameter]) {
  override def toString: String =
    parameters.map(_.toString).reduce((left, right) => left + ", " + right)
}

case class Parameter(parameterType: Type, identifier: Identifier) {
  override def toString: String =
    parameterType.toString + " " + identifier.toString
}

object Program {
  val build: (List[Function], Statement) => Program = Program(_, _)
}

object Function {
  val build: (Type, Identifier, Option[ParameterList], Statement) => Function =
    Function(_, _, _, _)
}

object ParameterList {
  val build: List[Parameter] => ParameterList = ParameterList(_)
}

object Parameter {
  val build: (Type, Identifier) => Parameter = Parameter(_, _)
}
