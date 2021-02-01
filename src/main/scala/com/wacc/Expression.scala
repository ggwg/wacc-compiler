package com.wacc

import com.wacc.operator.{BinaryOperator, UnaryOperator}

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight {}
sealed trait AssignmentLeft {}

case class ArrayElement(
    val identifier: Identifier,
    val expressions: List[Expression]
) extends Expression
    with AssignmentLeft

case class BinaryOperatorApplication(
    val expression1: Expression,
    val binaryOperator: BinaryOperator,
    val expression2: Expression
) extends Expression

case class BooleanLiter(val boolean: Boolean) extends Expression

case class CharacterLiter(val char: Char) extends Expression

case class Identifier(val identifier: String)
    extends Expression
    with AssignmentLeft

case class IntegerLiter(sign: Option[IntegerSign], val digits: List[Digit])
    extends Expression

case class PairLiter() extends Expression

case class StringLiter(val string: String) extends Expression

case class UnaryOperatorApplication(
    val unaryOperator: UnaryOperator,
    val expression: Expression
) extends Expression

case class ArrayLiter(val expressions: List[Expression]) extends AssignmentRight

case class FunctionCall(identifier: Identifier, arguments: Option[ArgumentList])
    extends AssignmentRight

case class NewPair(expression1: Expression, expression2: Expression)
    extends AssignmentRight

case class PairElement(val firstField: Expression, val secondField: Expression)
    extends AssignmentRight
    with AssignmentLeft

case class ArgumentList(val expressions: List[Expression])
