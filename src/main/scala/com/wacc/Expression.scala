package com.wacc

import com.wacc.operator._
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNodeVoid {}
sealed trait AssignmentLeft extends ASTNodeVoid {}

/* Class representing an unary operation (e.g. chr 101) */
case class UnaryOperatorApplication(operator: UnaryOperator, operand: Expression)(position: (Int, Int))
    extends Expression {
  override def toString: String = operator.toString + " " + operand.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* Get the operand type */

    val operandType = operand.getType(symbolTable)

    /* Error generation process */
    operator match {

      /* Chr accepts an integer parameter */
      case Chr() =>
        if (!operandType.unifies(IntType())) {
          errors += UnaryOperatorError("chr", "int", operandType.toString, operand.getPos())
          return
        }

      /* Negate accepts an integer parameter */
      case Negate() =>
        if (!operandType.unifies(IntType())) {
          errors += UnaryOperatorError("(-) (i.e. negate)", "int", operandType.toString, operand.getPos())
          return
        }

      /* Not accepts a boolean parameter */
      case Not() =>
        if (!operandType.unifies(BooleanType())) {
          errors += UnaryOperatorError("not", "boolean", operandType.toString, operand.getPos())
          return
        }

      /* Ord accepts a character parameter */
      case Ord() =>
        if (!operandType.unifies(CharacterType())) {
          errors += UnaryOperatorError("ord", "char", operandType.toString, operand.getPos())
          return
        }

      /* Length accepts an array */
      case Length() =>
        operandType match {
          case ArrayType(_) | EmptyType() => ()
          case _ =>
            errors += UnaryOperatorError("length", "array", operandType.toString, operand.getPos())
            return
        }
    }

    /* Check operand correctness */
    operand.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = operator match {
    case Chr() => CharacterType()
    case Not() => BooleanType()
    case _     => IntType()
  }
}

/* Represents a function call (e.g. call fun(1)) */
case class FunctionCall(name: Identifier, arguments: Option[ArgumentList])(position: (Int, Int))
    extends AssignmentRight {

  override def toString: String =
    "call " + name + "(" + (arguments match {
      case Some(args) => args.toString
      case None       => ""
    }) + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* Lookup the function's type using it's name across the entire symbol table hierarchy */
    val func: Option[(Type, ASTNode)] = symbolTable.lookupAll(name.identifier)

    if (func.isEmpty) {
      /* Invalid call to a function that's undefined */
      errors += DefaultError("Function " + name.identifier + " not defined in scope", name.getPos())
      return
    }

    func.get._2 match {

      case Function(returnType: Type, _: Identifier, params: Option[ParameterList], _: Statement) =>
        /* Extract the expected parameter types from the function signature */
        val expectedParams = {
          params match {
            case Some(list: ParameterList) => Some(list.parameters.map(parameter => parameter.parameterType))
            case None                      => None
          }
        }
        val expectedSignature = FunctionType(returnType, expectedParams)

        /* Extract the supplied parameter types from the function call */
        val calledParams = {
          arguments match {
            case Some(list: ArgumentList) => Some(list.expressions.map(expression => expression.getType(symbolTable)))
            case None                     => None
          }
        }
        val calledSignature = FunctionType(returnType, calledParams)

        /* Check that the supplied parameters match the signature */
        if (!expectedSignature.unifies(calledSignature)) {
          /* The signatures of the functions don't match - type mismatch in the caller arguments or return type */
          errors += DefaultError("Type mismatch for Functions with function in symbol table (TODO)", getPos())
          return
        }

        /* Check correctness of all arguments */
        arguments.foreach(_.check(symbolTable))
      case _ =>
        /* Invalid function call with an identifier that's not a function */
        errors +=
          DefaultError(
            "Function call " + name.identifier + " is not of type Function. Got of type " + func.get._1,
            getPos()
          )
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    symbolTable.lookupAll(name.identifier).getOrElse((VoidType(), null))._1
  }
}

/* Represents a list of arguments (e.g. (expr1, expr2)) */
case class ArgumentList(expressions: List[Expression]) extends ASTNodeVoid {
  override def toString: String = expressions.map(_.toString).reduce((left, right) => left + ", " + right)

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Check the correctness of each argument */
    expressions.foreach(_.check(symbolTable))
  }
}

/* Represents an array element access (e.g. ident[5][10]) */
case class ArrayElement(name: Identifier, expressions: List[Expression])(position: (Int, Int))
    extends Expression
    with AssignmentLeft {
  override def toString: String = name.toString + expressions.flatMap("[" + _.toString + "]")

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (expressions.isEmpty) {
      errors += DefaultError("No array index specified in attempt to access array " + name.identifier, getPos())
    } else {
      /* Go through each expression and check if it's of type int */
      expressions.foreach(expression => {
        val expressionType = expression.getType(symbolTable)
        if (!expressionType.unifies(IntType()))
          errors += DefaultError(
            "Array index expected type int, but found " + expressionType.getType(symbolTable).toString,
            getPos()
          )
      })

      /* Check correctness of each expression */
      expressions.foreach(_.check(symbolTable))
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    var identType = name.getType(symbolTable)

    /* Strip the [] until we find the array's type (e.g. int[][][] a, so a[1] has type int[][] */
    for (i <- 1 to expressions.length)
      identType match {
        case ArrayType(arrayType) => identType = arrayType
        case _                    => return VoidType()
      }
    identType
  }
}

/* Represents a binary operation (e.g. 1 + 2) */
case class BinaryOperatorApplication(leftOperand: Expression, operator: BinaryOperator, rightOperand: Expression)(
  position: (Int, Int)
) extends Expression {
  override def toString: String = leftOperand.toString + " " + operator.toString + " " + rightOperand.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* Extract the operand types */
    val leftType = leftOperand.getType(symbolTable)
    val rightType = rightOperand.getType(symbolTable)
    val op = operator.toString

    /* Error generation process */
    operator match {

      /* Integer operations expect integer parameters */
      case Add() | Divide() | Modulo() | Multiply() | Subtract() =>
        if (!leftType.unifies(IntType())) {
          errors += BinaryOperatorError(op, IntType.toString(), leftType.toString, position, isLeft = true)
          return
        } else if (!rightType.unifies(IntType())) {
          errors += BinaryOperatorError(op, IntType.toString(), rightType.toString, position, isLeft = false)
          return
        }

      /* Comparison operations expect integers or characters */
      case GreaterThan() | GreaterEqualThan() | SmallerThan() | SmallerEqualThan() =>
        val expected = IntType.toString() + " or " + CharacterType.toString()

        if (!(leftType.unifies(IntType()) || leftType.unifies(CharacterType()))) {
          errors += BinaryOperatorError(op, expected, leftType.toString, position, isLeft = true)
          return
        } else if (!(rightType.unifies(IntType()) || rightType.unifies(CharacterType()))) {
          errors += BinaryOperatorError(op, expected, rightType.toString, position, isLeft = false)
          return
        }

      /* Equal operations expect any same types */
      case Equals() | NotEquals() =>
        if (!leftType.unifies(rightType)) {
          errors += DefaultError(
            "Cannot compare " + leftType.toString + " and " + rightType.toString + " types " +
              " in " + this.toString,
            position
          )
          return
        }

      /* Boolean operations expect boolean types */
      case And() | Or() =>
        if (!leftType.unifies(BooleanType())) {
          errors += BinaryOperatorError(op, BooleanType.toString(), leftType.toString, position, isLeft = true)
          return
        } else if (!rightType.unifies(BooleanType())) {
          errors += BinaryOperatorError(op, BooleanType.toString(), rightType.toString, position, isLeft = false)
          return
        }
    }

    /* Check the operands */
    leftOperand.check(symbolTable)
    rightOperand.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type =
    operator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() => IntType()
      case _                                                     => BooleanType()
    }
}

/* Represents a bool (true or false) */
case class BooleanLiter(boolean: Boolean)(position: (Int, Int)) extends Expression {
  override def toString: String = boolean.toString
  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
}

/* Represents a character(e.g. 'a')*/
case class CharacterLiter(char: Char)(position: (Int, Int)) extends Expression {
  override def toString: String = "'" + char + "'"
  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = CharacterType()
}

/* Represents an identifier(e.g. int myIdentifier) */
case class Identifier(identifier: String)(position: (Int, Int)) extends Expression with AssignmentLeft {
  override def toString: String = identifier

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    if (getType(symbolTable).unifies(VoidType())) {
      /* Identifier was a void type - indicating that it hasn't been defined yet */
      errors += DefaultError("Undefined identifier \"" + identifier + "\"", position)
    }
  }

  override def getType(symbolTable: SymbolTable): Type =
    /* Look up the symbol table hierarchy for the type of the identifier using it's name */
    symbolTable.lookupAll(identifier).getOrElse((VoidType(), null))._1

  override def getPos(): (Int, Int) = position
}

/* Represents an integer (e.g. 1234 or -100) */
case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])(position: (Int, Int)) extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = IntType()
  override def check(symbolTable: SymbolTable)(implicit errors: ListBuffer[Error]): Unit = {

    /* Map the characters to digits */
    val intDigits = digits.map(_.digit - '0')
    var value: Long = 0

    /* Extract sign value of the number */
    var signValue = 1
    if (sign.nonEmpty && sign.get.sign == '-') signValue = -1

    /* Check that the number does not exceed the bounds */
    for (i <- intDigits) {
      value = (value * 10) + i
      if (value > Integer.MAX_VALUE && signValue == 1)
        errors += DefaultError("Max bound of int exceeded...", getPos())
      else if (-value < Integer.MIN_VALUE && signValue == -1)
        errors += DefaultError("Min bound of int exceeded...", getPos())
    }
  }
}

/* Represents a string (e.g. "Hello, World!") */
case class StringLiter(string: String)(position: (Int, Int)) extends Expression {
  override def toString: String = "\"" + string + "\""
  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = StringType()
}

/* Represents an array literal(e.g. [1, 2, 3, 4]) */
case class ArrayLiter(expressions: List[Expression])(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduceOption((left, right) => left + ", " + right)
    .getOrElse("") + "]"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (expressions.nonEmpty) {
      /* Get the type of the first element */
      val arrayElementType = expressions.head.getType(symbolTable)

      /* All other elements must have the same type */
      for (expression <- expressions.tail) {
        if (!expression.getType(symbolTable).unifies(arrayElementType)) {
          errors += DefaultError(
            "Array assignment type mismatch: Got type " + expression.getType(
              symbolTable
            ) + ", Expected " + arrayElementType,
            expression.getPos()
          )
          return
        }
      }

      /* Check correctness of all expressions */
      expressions.foreach(_.check(symbolTable))
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    /* Empty type if there are no expression in the array literal.
       Thus it can match any array type */
    if (expressions.isEmpty) EmptyType()
    else ArrayType(expressions.head.getType(symbolTable))
  }
}

/* Represents the null literal */
case class PairLiter()(position: (Int, Int)) extends Expression {
  override def toString: String = "null"
  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = NullType()
}

/* Represents creation of a new pair (e.g. newPair(1, "one")) */
case class NewPair(first: Expression, second: Expression)(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "newpair(" + first.toString + ", " + second.toString + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Check correctness of the pair elements */
    first.check(symbolTable)
    second.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    /* Extract the pair element types */
    val fstType: Type = first.getType(symbolTable)
    val sndType: Type = second.getType(symbolTable)

    /* If any pair element type had an error, the whole pair will be an error as well */
    fstType match {
      case VoidType() => return VoidType()
      case _          => ()
    }
    sndType match {
      case VoidType() => return VoidType()
      case _          => ()
    }

    PairType(asPairElementType(fstType), asPairElementType(sndType))
  }

  /* Convert any type to the corresponding pair element type */
  private def asPairElementType(typ: Type): PairElementType = {
    typ match {
      case baseType: BaseType          => baseType
      case PairType(_, _) | NullType() => PairDefault()
      case ArrayType(arrayType)        => ArrayType(arrayType)
      case EmptyType()                 => EmptyType()
      case _                           => VoidType()
    }
  }
}

/* Represents the access of a pair (e.g. fst myPair, snd x) */
case class PairElement(expression: Expression, isFirst: Boolean)(position: (Int, Int))
    extends AssignmentRight
    with AssignmentLeft {
  override def toString: String = (if (isFirst) "fst " else "snd ") + expression.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* The expression must be a pair */
    expression.getType(symbolTable) match {
      case PairType(_, _) => ()
      case _              => errors += DefaultError("expression is not a pair", expression.getPos())
    }

    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    expression.getType(symbolTable) match {

      /* Expression is a pair */
      case PairType(fstType, sndType) =>
        if (isFirst) fstType.getType(symbolTable)
        else sndType.getType(symbolTable)

      /* Otherwise, it is invalid */
      case _ => VoidType()
    }
  }
}

/*  -----------------------------  Class objects  -----------------------------  */

/* ✅ Done */
object ArrayElement {
  def apply(identifier: Parsley[Identifier], expressions: Parsley[List[Expression]]): Parsley[ArrayElement] =
    pos <**> (identifier, expressions).map(ArrayElement(_, _))
}

/* ✅ Done */
object ArrayLiter {
  def apply(maybeExpressions: Parsley[Option[(Expression, List[Expression])]]): Parsley[ArrayLiter] =
    pos <**> maybeExpressions.map {
      case Some((expression, expressions)) => ArrayLiter(expression :: expressions)
      case None                            => ArrayLiter(List())
    }
}

/* ✅ Done */
object BinaryOperatorApplication {
  def apply(
    leftOperand: Parsley[Expression],
    operator: Parsley[BinaryOperator],
    rightOperand: Parsley[Expression]
  ): Parsley[BinaryOperatorApplication] =
    pos <**> (leftOperand, operator, rightOperand).map(BinaryOperatorApplication(_, _, _))
}

/* ✅ Done */
object BooleanLiter {
  def apply(bool: Parsley[String]): Parsley[BooleanLiter] = pos <**> bool.map(bool => BooleanLiter(bool.equals("true")))
}

/* ✅ Done */
object CharacterLiter {
  def apply(character: Parsley[DefaultCharacter]): Parsley[CharacterLiter] =
    pos <**> character.map(character => CharacterLiter(character.char))
}

/* ✅ Done */
object Identifier {
  def apply(prefix: Parsley[String], suffix: Parsley[List[Char]]): Parsley[Identifier] =
    pos <**> (prefix, suffix).map((prefix, suffix) => Identifier(prefix + suffix.mkString))
}

/* ✅ Done */
object IntegerLiter {
  def apply(sign: Parsley[Option[IntegerSign]], digits: Parsley[List[Digit]]): Parsley[IntegerLiter] =
    pos <**> (sign, digits).map(IntegerLiter(_, _))
}

/* ✅ Done */
object PairLiter {
  def apply(string: Parsley[String]): Parsley[PairLiter] = pos <**> string.map(_ => PairLiter())
}

/* ✅ Done */
object StringLiter {
  def apply(characters: Parsley[List[DefaultCharacter]]): Parsley[StringLiter] =
    pos <**> characters.map(character => StringLiter(character.mkString))
}

/* ✅ Done */
object UnaryOperatorApplication {
  def apply(operator: Parsley[UnaryOperator], operand: Parsley[Expression]): Parsley[UnaryOperatorApplication] =
    pos <**> (operator, operand).map(UnaryOperatorApplication(_, _))
}

/* ✅ Done */
object FunctionCall {
  def apply(name: Parsley[Identifier], arguments: Parsley[Option[ArgumentList]]): Parsley[FunctionCall] =
    pos <**> (name, arguments).map(FunctionCall(_, _))
}

/* ✅ Done */
object NewPair {
  def apply(first: Parsley[Expression], second: Parsley[Expression]): Parsley[NewPair] =
    pos <**> (first, second).map(NewPair(_, _))
}

/* ✅ Done */
object PairElement {
  def apply(expression: Parsley[Expression], isFirst: Boolean): Parsley[PairElement] =
    pos <**> expression.map(PairElement(_, isFirst))
}

/* ✅ Done */
object ArgumentList {
  def apply(expression: Parsley[Expression], expressions: Parsley[List[Expression]]): Parsley[ArgumentList] =
    (expression, expressions).map((e, es) => ArgumentList(e :: es))
}
