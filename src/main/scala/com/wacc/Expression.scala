package com.wacc

import com.wacc.operator._

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNodeVoid {}
sealed trait AssignmentLeft extends ASTNodeVoid {}

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
  // TODO: what type to return if ArrayElement is empty? (how to define getType function)
}

case class BinaryOperatorApplication(
    expression1: Expression,
    binaryOperator: BinaryOperator,
    expression2: Expression
) extends Expression {
  override def toString: String =
    expression1.toString + " " + binaryOperator.toString + " " + expression2.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BINARY-OPERATOR-APPLICATION CHECK")

    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() =>
        if (
          expression1.check(symbolTable).getClass == IntType.getClass
          && expression2.check(symbolTable).getClass == IntType.getClass
        ) {
          return IntType
        } else {
          println(
            "ERROR: INTEGER BIN-OP, expected type Int for " + binaryOperator,
            getClass.toString
          )
          return ()
        }
      case GreaterThan() | GreaterEqualThan() | SmallerThan() |
          SmallerEqualThan() =>
        if (
          (expression1.check(symbolTable).getClass == IntType.getClass
            || expression1
                                      .check(symbolTable)
              .getClass == CharacterType.getClass)
          && (expression2.check(symbolTable).getClass == IntType.getClass
            || expression2
                                      .check(symbolTable)
              .getClass == CharacterType.getClass)
        ) {
          return BooleanType
        } else {
          println(
            "ERROR: COMPARIONS/EQUALS BIN-OP, expected type Int/Char for " + binaryOperator,
            getClass.toString
          )
          return ()
        }
      case Equals() | NotEquals() =>
        return BooleanType
      case And() | Or() =>
        if (
          expression1.check(symbolTable).getClass == BooleanType.getClass
          && expression2.check(symbolTable).getClass == BooleanType.getClass
        ) {
          return BooleanType
        } else {
          println(
            "ERROR: AND/OR BIN-OP, expected type Bool for " + binaryOperator,
            getClass.toString
          )
          return ()
        }
    }
    // In case we add more unary operators
    return ()
  }
}

case class BooleanLiter(boolean: Boolean) extends Expression {
  override def toString: String = boolean.toString
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BOOLEAN-LITER CHECK")
  }
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
}

case class CharacterLiter(char: Char) extends Expression {
  override def toString: String = "'" + char + "'"
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE CHARACTER-LITER CHECK")
  }

  override def getType(symbolTable: SymbolTable): Type = CharacterType()
}

case class Identifier(identifier: String)
    extends Expression
    with AssignmentLeft {
  override def toString: String = identifier.toString
  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IDENTIFIER CHECK")
    // lookup identifier in symbol table, and extract base type
    // return the type as specified in the symbol table
  }
  override def getType(symbolTable: SymbolTable): Type = StringType()
}

case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])
    extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE INTEGER-LITER CHECK")
    return IntType
  }
  override def getType(symbolTable: SymbolTable): Type = IntType()
}

case class PairLiter() extends Expression {
  override def toString: String = "null"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE PAIR-LITER CHECK")
  }

  // TODO: What should type of PairLiter be?
}

case class StringLiter(string: String) extends Expression {
  override def toString: String = "\"" + string + "\""

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE STRING-LITER CHECK")
    return StringType
  }

  override def getType(symbolTable: SymbolTable): Type = StringType()
}

case class UnaryOperatorApplication(
    unaryOperator: UnaryOperator,
    expression: Expression
) extends Expression {
  override def toString: String = unaryOperator.toString + expression.toString

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE UNARY-OPERATOR-APPLICATION CHECK")

    unaryOperator match {
      case Chr() =>
        if (expression.check(symbolTable).getClass == IntType.getClass) {
          // TODO: check that expression is an int between 0-255
          return CharacterType
        } else {
          println("ERROR CHR")
          return ()
        }
      case Length() =>
        //TODO: NOT DEFINED ARRAYS YET
        println("NOT DEFINED LENGTH FOR ARRAYS YET")
        return ()
      case Negate() =>
        if (expression.check(symbolTable).getClass != IntType.getClass) {
          println("ERROR NEGATE")
          return ()
        } else {
          return IntType
        }
      case Not() =>
        if (expression.check(symbolTable).getClass != BooleanType.getClass) {
          println("ERROR NOT")
          return ()
        } else {
          return BooleanType
        }
      case Ord() =>
        if (expression.check(symbolTable).getClass != IntType.getClass) {
          println("ERROR ORD")
          return ()
        } else {
          return IntType
        }
    }
    // In case we add more unary operators
    return ()
  }
}

case class ArrayLiter(expressions: List[Expression]) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduce((left, right) => left + ", " + right) + "]"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ARRAY-LITER CHECK")
  }
}

case class FunctionCall(identifier: Identifier, arguments: Option[ArgumentList])
    extends AssignmentRight {
  override def toString: String =
    "call " + identifier + "(" + (arguments match {
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
    "newpair(" + expression1.toString + ", " + expression2.toString + ")"

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

case class ArgumentList(expressions: List[Expression]) extends ASTNodeVoid {
  override def toString: String =
    expressions.map(_.toString).reduce((left, right) => left + ", " + right)

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
