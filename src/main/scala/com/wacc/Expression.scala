package com.wacc

import com.wacc.operator.{BinaryOperator, UnaryOperator}

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight {}
sealed trait AssignmentLeft {}

case class ArrayElement(
    identifier: Identifier,
    expressions: List[Expression]
) extends Expression
    with AssignmentLeft

case class BinaryOperatorApplication(
    expression1: Expression,
    binaryOperator: BinaryOperator,
    expression2: Expression
) extends Expression

case class BooleanLiter(boolean: Boolean) extends Expression

case class CharacterLiter(char: Char) extends Expression

case class Identifier(identifier: String) extends Expression with AssignmentLeft

case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])
    extends Expression

case class PairLiter() extends Expression

case class StringLiter(string: String) extends Expression

case class UnaryOperatorApplication(
    unaryOperator: UnaryOperator,
    expression: Expression
) extends Expression

case class ArrayLiter(expressions: List[Expression]) extends AssignmentRight

case class FunctionCall(identifier: Identifier, arguments: Option[ArgumentList])
    extends AssignmentRight

case class NewPair(expression1: Expression, expression2: Expression)
    extends AssignmentRight

case class PairElement(firstField: Expression, secondField: Expression)
    extends AssignmentRight
    with AssignmentLeft

case class ArgumentList(expressions: List[Expression])
