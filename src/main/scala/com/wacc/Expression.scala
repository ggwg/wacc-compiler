package com.wacc

import com.wacc.operator._

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNodeVoid {}
sealed trait AssignmentLeft extends ASTNodeVoid {}

/* Since array element may be empty, the type may either be ArrayType(SomeType()) or ArrayType(VoidType()) */
case class ArrayElement(
    identifier: Identifier,
    expressions: List[Expression]
) extends Expression
    with AssignmentLeft {
  override def toString: String =
    identifier.toString + expressions.flatMap("[" + _.toString + "]")

  override def check(symbolTable: SymbolTable): Unit = {
    // If the list is non-empty, check that all the types of the elements of the array
    // are the same.
    if (!expressions.isEmpty) {
      val arrayElementType = expressions.head.getType(symbolTable)
      for (expression <- expressions) {
        if (!expression.getType(symbolTable).unifies(arrayElementType)) {
          println(
            "Array type not equal to expected array type. Got: " + expression
              .getType(symbolTable)
              + ", Expected: " + arrayElementType
          )
        }
      }
    }
  }
  override def getType(symbolTable: SymbolTable): PairElementType = {
    if (expressions.isEmpty) {
      ArrayType(VoidType())
    } else {
      ArrayType(expressions.head.getType(symbolTable))
    }
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
        if (!expression1.getType(symbolTable).unifies(IntType())) {
          println("ERROR: INTEGER BIN-OP, expected type Int")
        }
        if (!expression2.getType(symbolTable).unifies(IntType())) {
          println("ERROR: INTEGER BIN-OP, expected type Int")
        }
      case GreaterThan() | GreaterEqualThan() | SmallerThan() |
          SmallerEqualThan() =>
        val result1 = expression1.getType(symbolTable).unifies(IntType()) ||
          expression1.getType(symbolTable).unifies(CharacterType())
        val result2 = expression2.getType(symbolTable).unifies(IntType()) ||
          expression2.getType(symbolTable).unifies(CharacterType())
        if (!result1) {
          println("ERROR: COMPARIONS/EQUALS BIN-OP, expected type Int/Char")
        }
        if (!result2) {
          println("ERROR: COMPARIONS/EQUALS BIN-OP, expected type Int/Char")
        }
      case Equals() | NotEquals() =>
        println("BIN-OP, nothing to do for equals")
      case And() | Or() =>
        if (!expression1.getType(symbolTable).unifies(BooleanType())) {
          println("ERROR: AND/OR BIN-OP, expected type Bool")
        }
        if (!expression2.getType(symbolTable).unifies(BooleanType())) {
          println("ERROR: AND/OR BIN-OP, expected type Bool")
        }
    }
  }

  override def getType(symbolTable: SymbolTable): PairElementType = {
    println("getType(): BINARY-OPERATOR-APPLICATION")
    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() => IntType()
      case _                                                     => BooleanType()
    }
  }
}

case class BooleanLiter(boolean: Boolean) extends Expression {
  override def toString: String = boolean.toString
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE BOOLEAN-LITER CHECK")
  }
  override def getType(symbolTable: SymbolTable): PairElementType =
    BooleanType()
}

case class CharacterLiter(char: Char) extends Expression {
  override def toString: String = "'" + char + "'"
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE CHARACTER-LITER CHECK")
  }

  override def getType(symbolTable: SymbolTable): PairElementType =
    CharacterType()
}

/*
assignment:
x = 5
assignment.check()
compare types
x.getType() -> VoidType
5.getTpye() -> IntType
Different
x.check()
 */
case class Identifier(identifier: String)
    extends Expression
    with AssignmentLeft {
  override def toString: String = identifier.toString
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE IDENTIFIER CHECK")
    if (getType(symbolTable).unifies(VoidType())) {
      println("Error - undefined identifier")
    }
  }
  override def getType(symbolTable: SymbolTable): PairElementType = {
    var lookupVal: Option[(PairElementType, ASTNode)] =
      symbolTable.lookupAll(identifier)
    return lookupVal.getOrElse((VoidType(), null))._1
  }
}

case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])
    extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  override def getType(symbolTable: SymbolTable): PairElementType = IntType()
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
  override def getType(symbolTable: SymbolTable): PairElementType = StringType()
}

case class UnaryOperatorApplication(
    unaryOperator: UnaryOperator,
    expression: Expression
) extends Expression {
  override def toString: String = unaryOperator.toString + expression.toString

  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE UNARY-OPERATOR-APPLICATION CHECK")
    unaryOperator match {
      case Chr() =>
        if (expression.getType(symbolTable).unifies(IntType())) {
          // TODO: check that expression is an int between 0-255
          expression.check(symbolTable)
        } else {
          println("ERROR IN UNARY-OPERATOR APPLICATION: CHR")
        }
      case Length() =>
      // TODO: Type checking for arrays
      // if (expression.getType(symbolTable).unifies(ArrayType()))
      case Negate() =>
        if (expression.getType(symbolTable).unifies(IntType())) {
          expression.check(symbolTable)
        } else {
          println("ERROR NEGATE")
        }
      case Not() =>
        if (expression.getType(symbolTable).unifies(BooleanType())) {
          expression.check(symbolTable)
        } else {
          println("ERROR NOT")
        }
      case Ord() =>
        if (expression.getType(symbolTable).unifies(CharacterType())) {
          println("ERROR ORD")
          return ()
        } else {
          return IntType
        }
    }
    // In case we add more unary operators
    return ()
  }
  override def getType(symbolTable: SymbolTable): PairElementType = {
    println("getType(): BINARY-OPERATOR-APPLICATION")
    unaryOperator match {
      case Chr() => CharacterType()
      case Not() => BooleanType()
      case _     => IntType()
    }
  }
}

case class ArrayLiter(expressions: List[Expression]) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduceOption((left, right) => left + ", " + right)
    .getOrElse("") + "]"

  // TODO:
  override def check(symbolTable: SymbolTable): Unit = {
    for (expression <- expressions) {
      expression.check(symbolTable)
    }
  }
  // If there are expressions then we will return ArrayType(VoidType) - this case needs to be
  // caught in type checking
  override def getType(symbolTable: SymbolTable): PairElementType = {
    if (expressions.isEmpty) {
      ArrayType(VoidType())
    } else {
      ArrayType(expressions.head.getType(symbolTable))
    }
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
    var func: Option[(PairElementType, ASTNode)] =
      symbolTable.lookupAll(identifier.identifier)
    if (func.isEmpty) {
      print("Error - unknown function name")
    }
  }
}

/* Check done */
case class NewPair(expression1: Expression, expression2: Expression)
    extends AssignmentRight {
  override def toString: String =
    "newpair(" + expression1.toString + ", " + expression2.toString + ")"

  // pair(int, char) x = newpair(10, 'c')
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE NEW-PAIR CHECK")
    expression1.check(symbolTable)
    expression2.check(symbolTable)
  }
  override def getType(symbolTable: SymbolTable): PairElementType = {
    PairType(expression1.getType(symbolTable), expression2.getType(symbolTable))
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

/*  -----------------------------  Class objects  -----------------------------  */

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
  val buildKeywordPrefix: (String, List[Char]) => Identifier =
    (prefix, suffix) => Identifier(prefix + suffix.mkString)

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
