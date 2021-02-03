package com.wacc

import com.wacc.operator.{
  BinaryOperator,
  Chr,
  Length,
  Negate,
  Not,
  Ord,
  UnaryOperator
}

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNode {}
sealed trait AssignmentLeft extends ASTNode {}

case class ArrayElement(
    identifier: Identifier,
    expressions: List[Expression]
) extends Expression
    with AssignmentLeft {
  override def toString: String =
    identifier.toString + expressions.flatMap("[" + _.toString + "]")

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ARRAY-ELEMENT CHECK")
  }
}

case class BinaryOperatorApplication(
    expression1: Expression,
    binaryOperator: BinaryOperator,
    expression2: Expression
) extends Expression {
  override def toString: String =
    expression1.toString + binaryOperator.toString + expression2.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BINARY-OPERATOR CHECK")
  }
}

case class BooleanLiter(boolean: Boolean) extends Expression {
  override def toString: String = boolean.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BOOLEAN-LITER CHECK")
  }
}

case class CharacterLiter(char: Char) extends Expression {
  override def toString: String = "'" + char + "'"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE CHARACTER-LITER CHECK")
  }
}

case class Identifier(identifier: String)
    extends Expression
    with AssignmentLeft {
  override def toString: String = identifier

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IDENTIFIER CHECK")
  }
}

case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])
    extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE INTEGER-LITER CHECK")
  }
}

case class PairLiter() extends Expression {
  override def toString: String = "null"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PAIR-LITER CHECK")
  }
}

case class StringLiter(string: String) extends Expression {
  override def toString: String = string

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE STRING-LITER CHECK")
  }
}

case class UnaryOperatorApplication(
    unaryOperator: UnaryOperator,
    expression: Expression
) extends Expression {
  override def toString: String = unaryOperator match {
    case Chr() | Length() | Negate() => "(" + expression.toString + ")"
    case Not() | Ord()               => expression.toString
  }

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE UNARY-OPERATOR-APPLICATION CHECK")
  }
}

case class ArrayLiter(expressions: List[Expression]) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduce((left, right) => left + "," + right) + "]"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ARRAY-LITER CHECK")
  }
}

case class FunctionCall(identifier: Identifier, arguments: Option[ArgumentList])
    extends AssignmentRight {
  override def toString: String = "call" + identifier + "(" + (arguments match {
    case Some(args) => args.toString
    case None       => ""
  }) + ")"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE FUNCTION-CALL CHECK")
  }
}

case class NewPair(expression1: Expression, expression2: Expression)
    extends AssignmentRight {
  override def toString: String =
    "newpair(" + expression1.toString + "," + expression2.toString + ")"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE NEW-PAIR CHECK")
  }
}

case class PairElement(expression: Expression, isFirst: Boolean)
    extends AssignmentRight
    with AssignmentLeft {
  override def toString: String =
    (if (isFirst) "fst " else "snd ") + expression.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PAIR-ELEMENT CHECK")
  }
}

case class ArgumentList(expressions: List[Expression])
    extends ASTNode {
  override def toString: String =
    expressions.map(_.toString).reduce((left, right) => left + "," + right)

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ARGUMENT-LIST CHECK")
  }
}

object ArrayElement {
  val build: (Identifier, List[Expression]) => ArrayElement = ArrayElement(_, _)
}

object ArrayLiter {
  val build: (Option[(Expression, List[Expression])] => ArrayLiter) = {
    case None => ArrayLiter(List())
    case Some((e, es)) =>
      ArrayLiter(e :: es)
  }
}

object BinaryOperatorApplication {
  val build
      : (Expression, BinaryOperator, Expression) => BinaryOperatorApplication =
    (e1, op, e2) => BinaryOperatorApplication(e1, op, e2)
}

object BooleanLiter {
  val build: (String => BooleanLiter) = bool =>
    BooleanLiter(bool.equals("true"))
}

object CharacterLiter {
  val build: (DefaultCharacter => CharacterLiter) = chr =>
    CharacterLiter(chr.char)
}

object Identifier {
  val build: (Char, List[Char]) => Identifier = (letter, letters) =>
    Identifier((letter :: letters).mkString)
}

object IntegerLiter {
  val build: (Option[IntegerSign], List[Digit]) => IntegerLiter =
    IntegerLiter(_, _)
}

object PairLiter {
  val build: (String => PairLiter) = _ => PairLiter()
}

object StringLiter {
  val build: (List[DefaultCharacter] => StringLiter) = dcs =>
    StringLiter(dcs.mkString)
}

object UnaryOperatorApplication {
  val build: (UnaryOperator, Expression) => UnaryOperatorApplication =
    UnaryOperatorApplication(_, _)
}

object FunctionCall {
  val build: (Identifier, Option[ArgumentList]) => FunctionCall =
    FunctionCall(_, _)
}

object NewPair {
  val build: (Expression, Expression) => NewPair = NewPair(_, _)
}

object PairElement {
  val build: (Expression, Boolean) => PairElement = PairElement(_, _)
}

object ArgumentList {
  val build: (Expression, List[Expression]) => ArgumentList = (e, es) =>
    ArgumentList(e :: es)
}
