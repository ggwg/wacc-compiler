package com.wacc

import com.wacc.operator._
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNodeVoid {}
sealed trait AssignmentLeft extends ASTNodeVoid {}

/* TODO:
   - Type checking for arrays (length) -> if (expression.getType(symbolTable).unifies(ArrayType()))
 */
case class UnaryOperatorApplication(operator: UnaryOperator, operand: Expression)(position: (Int, Int))
    extends Expression {
  override def toString: String = operator.toString + " " + operand.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking unary operator application...")
    val operandType = operand.getType(symbolTable)

    /* Error generation process */
    operator match {
      case Chr() =>
        if (operandType.unifies(IntType())) operand.check(symbolTable)
        else errors += UnaryOperatorError("chr", "int", operandType.toString, position)
      case Negate() =>
        if (operandType.unifies(IntType())) operand.check(symbolTable)
        else errors += UnaryOperatorError("(-) (i.e. negate)", "int", operandType.toString, position)
      case Not() =>
        if (operandType.unifies(BooleanType())) operand.check(symbolTable)
        else errors += UnaryOperatorError("not", "boolean", operandType.toString, position)
      case Ord() =>
        if (operandType.unifies(CharacterType())) operand.check(symbolTable)
        else errors += UnaryOperatorError("ord", "char", operandType.toString, position)
      case Length() => /* Implement this */
    }

    operand.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = operator match {
    case Chr() => CharacterType()
    case Not() => BooleanType()
    case _     => IntType()
  }
}

/* TODO:
   - Find out what to do for the check (returns List.empty for now)
 */
case class PairLiter()(position: (Int, Int)) extends Expression {
  override def toString: String = "null"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking pair literal...")
    errors += DefaultError("Pair Literal Check undefined...", position)
  }

  override def getPos(): (Int, Int) = position
}

/* TODO
   - Fix error messages
 */
case class FunctionCall(name: Identifier, arguments: Option[ArgumentList])(position: (Int, Int))
    extends AssignmentRight {
  override def toString: String =
    "call " + name + "(" + (arguments match {
      case Some(args) => args.toString
      case None       => ""
    }) + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking function call...")
    /* Lookup the function's type using it's name across the entire symbol table hierarchy */
    val func: Option[(Type, ASTNode)] = symbolTable.lookupAll(name.identifier)

    if (func.isEmpty) {
      /* Invalid call to a function that's undefined */
      errors += DefaultError("Function " + name.identifier + " not defined in scope", position)
      return
    }

    /* TODO
       - Perform check on each argument of the function call */
    func.get._2 match {
      case Function(returnType: Type, name: Identifier, params: Option[ParameterList], _: Statement) =>
        val expectedParams = {
          params match {
            case Some(list: ParameterList) => Some(list.parameters.map(parameter => parameter.parameterType))
            case None                      => None
          }
        }
        val expectedSignature = FunctionType(returnType, expectedParams)

        val calledParams = {
          arguments match {
            case Some(list: ArgumentList) => Some(list.expressions.map(expression => expression.getType(symbolTable)))
            case None                     => None
          }
        }
        val calledSignature = FunctionType(returnType, calledParams)

        if (!expectedSignature.unifies(calledSignature)) {
          /* The signatures of the functions don't match - type mismatch in the caller arguments or return type */
          errors += DefaultError("Type mismatch for Functions with function in symbol table (TODO)", position)
          return
        }

        arguments.foreach(_.check(symbolTable))
      case _ =>
        /* Invalid function call with a type that's not a function */
        errors +=
          DefaultError(
            "Function call " + name.identifier + " is not of type Function. Got of type " + func.get._1,
            position
          )
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    symbolTable.lookupAll(name.identifier).getOrElse((VoidType(), null))._1
  }
}

/* TODO:
   - Implement argument list checking
   - Implement the getType for this
 */
case class ArgumentList(expressions: List[Expression])(position: (Int, Int)) extends ASTNodeVoid {
  override def toString: String = expressions.map(_.toString).reduce((left, right) => left + ", " + right)

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking argument list...")
    for (expression <- expressions) {
      expression.check(symbolTable)
    }
  }

  override def getPos(): (Int, Int) = position
}

/* TODO:
   - Implement the getType override
   - Also try to provide more information about which index access attempt has the error
 */
case class ArrayElement(name: Identifier, expressions: List[Expression])(position: (Int, Int))
    extends Expression
    with AssignmentLeft {
  override def toString: String =
    name.toString + expressions.flatMap("[" + _.toString + "]")

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    val pos = (39, 15)

    if (expressions.isEmpty) {
      errors += DefaultError("No array index specified in attempt to access array " + name.identifier, pos)
    } else {
      /* Go through each expression and check if it's of type int and recursively call check on each one */
      for (expression <- expressions) {
        val expressionType = expression.getType(symbolTable)
        if (!expressionType.unifies(IntType()))
          errors += DefaultError("Array index expected type int, but found " + expressionType.toString, pos)
        expression.check(symbolTable)
      }
    }
  }

  override def getPos(): (Int, Int) = position
}

/* ✅ Check done */
case class BinaryOperatorApplication(leftOperand: Expression, binaryOperator: BinaryOperator, rightOperand: Expression)(
  position: (Int, Int)
) extends Expression {
  override def toString: String = leftOperand.toString + " " + binaryOperator.toString + " " + rightOperand.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking binary operator application")
    val leftType = leftOperand.getType(symbolTable)
    val rightType = rightOperand.getType(symbolTable)
    val op = binaryOperator.toString
    val pos = (39, 15)

    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() =>
        println(leftType)
        println(rightType)
        if (!leftType.unifies(IntType())) {
          errors += BinaryOperatorError(op, IntType.toString(), leftType.toString, pos, isLeft = true)
        } else if (!rightType.unifies(IntType())) {
          errors += BinaryOperatorError(op, IntType.toString(), rightType.toString, pos, isLeft = false)
        } else {
          leftOperand.check(symbolTable)
          rightOperand.check(symbolTable)
        }

      case GreaterThan() | GreaterEqualThan() | SmallerThan() | SmallerEqualThan() =>
        val expected = IntType.toString() + " or " + CharacterType.toString()

        if (!(leftType.unifies(IntType()) || leftType.unifies(CharacterType()))) {
          errors += BinaryOperatorError(op, expected, leftType.toString, pos, isLeft = true)
        } else if (!(rightType.unifies(IntType()) || rightType.unifies(CharacterType()))) {
          errors += BinaryOperatorError(op, expected, rightType.toString, pos, isLeft = false)
        } else {
          leftOperand.check(symbolTable)
          rightOperand.check(symbolTable)
        }

      case Equals() | NotEquals() =>
        if (!leftType.unifies(rightType)) {
          errors += DefaultError(
            "Cannot compare " + leftType.toString + " and " + rightType.toString + " types " +
              " in " + this.toString,
            pos
          )
        } else {
          leftOperand.check(symbolTable)
          rightOperand.check(symbolTable)
        }

      case And() | Or() =>
        println("GOT INSIDE AND OR")
        println(leftOperand.getType(symbolTable))
        println(rightOperand.getType(symbolTable))
        if (!leftType.unifies(BooleanType())) {
          println("GOT HERE 1")
          errors += BinaryOperatorError(op, BooleanType.toString(), leftType.toString, pos, isLeft = true)
        } else if (!rightType.unifies(BooleanType())) {
          println("GOT HERE 2")
          errors += BinaryOperatorError(op, BooleanType.toString(), rightType.toString, pos, isLeft = false)
        } else {
          println("GOT HERE 3")
          leftOperand.check(symbolTable)
          rightOperand.check(symbolTable)
        }
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type =
    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() => IntType()
      case _                                                     => BooleanType()
    }
}

/* ✅ Check done */
case class BooleanLiter(boolean: Boolean)(position: (Int, Int)) extends Expression {
  override def toString: String = boolean.toString

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = BooleanType()
}

/* ✅ Check done */
case class CharacterLiter(char: Char)(position: (Int, Int)) extends Expression {
  override def toString: String = "'" + char + "'"

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = CharacterType()
}

/* ✅ Check done */
case class Identifier(identifier: String)(position: (Int, Int)) extends Expression with AssignmentLeft {
  override def toString: String = identifier

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking identifier...")
    val pos = (39, 15)
    if (getType(symbolTable).unifies(VoidType())) List(DefaultError("Undefined identifier \"" + identifier + "\"", pos))
    else List.empty
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type =
    symbolTable.lookupAll(identifier).getOrElse((VoidType(), null))._1
}

/* ✅ Check done */
case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit])(position: (Int, Int)) extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  override def getPos(): (Int, Int) = position

  // TODO: Does BigInt check need to go here?

  override def getType(symbolTable: SymbolTable): Type = IntType()
}

/* ✅ Check done */
case class StringLiter(string: String)(position: (Int, Int)) extends Expression {
  override def toString: String = "\"" + string + "\""

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = StringType()
}

/* ✅ Check done */
case class ArrayLiter(expressions: List[Expression])(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduceOption((left, right) => left + ", " + right)
    .getOrElse("") + "]"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit =
    for (expression <- expressions) {
      expression.check(symbolTable)
    }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    if (expressions.isEmpty) {
      ArrayType(VoidType())
    } else {
      ArrayType(expressions.head.getType(symbolTable))
    }
  }
}

/* ✅ Check done */
case class NewPair(first: Expression, second: Expression)(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "newpair(" + first.toString + ", " + second.toString + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking newpair...")
    first.check(symbolTable)
    second.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    val fstType: PairElementType = asPairElementType(first.getType(symbolTable))
    val sndType: PairElementType = asPairElementType(second.getType(symbolTable))

    fstType match {
      case VoidType() => return VoidType()
      case _          => ()
    }
    sndType match {
      case VoidType() => return VoidType()
      case _          => ()
    }
    PairType(fstType, sndType)
  }

  private def asPairElementType(typ: Type): PairElementType = {
    typ match {
      case baseType: BaseType                   => baseType
      case PairType(elementType1, elementType2) => PairDefault()
      case ArrayType(arrayType)                 => ArrayType(arrayType)
      case VoidType()                           => VoidType()
      case FunctionType(returnType, parameters) => VoidType()
    }
  }
}

/* ✅ Check done */
case class PairElement(expression: Expression, isFirst: Boolean)(position: (Int, Int))
    extends AssignmentRight
    with AssignmentLeft {
  override def toString: String = (if (isFirst) "fst " else "snd ") + expression.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking pair element...")
    expression.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    expression.getType(symbolTable) match {
      case PairType(elementType1, elementType2) =>
        if (isFirst) {
          elementType1.getType(symbolTable)
        } else {
          elementType2.getType(symbolTable)
        }
      case _ => VoidType()
    }
    // expression.getType(symbolTable)
  }
}

/*  -----------------------------  Class objects  -----------------------------  */

object ArrayElement {
  def apply(ident: Parsley[Identifier], exprs: Parsley[List[Expression]]): Parsley[ArrayElement] =
    pos <**> (ident, exprs).map(ArrayElement(_, _))
}

object ArrayLiter {
  def apply(option: Parsley[Option[(Expression, List[Expression])]]): Parsley[ArrayLiter] =
    pos <**> option.map {
      case None          => ArrayLiter(List())
      case Some((e, es)) => ArrayLiter(e :: es)
    }
}

object BinaryOperatorApplication {
  def apply(
    expr1: Parsley[Expression],
    operator: Parsley[BinaryOperator],
    expr2: Parsley[Expression]
  ): Parsley[BinaryOperatorApplication] =
    pos <**> (expr1, operator, expr2).map(BinaryOperatorApplication(_, _, _))
}

object BooleanLiter {
  def apply(bool: Parsley[String]): Parsley[BooleanLiter] = pos <**> bool.map(b => BooleanLiter(b.equals("true")))
}

object CharacterLiter {
  def apply(chr: Parsley[DefaultCharacter]): Parsley[CharacterLiter] = pos <**> chr.map(c => CharacterLiter(c.char))
}

object Identifier {
  def apply(prefix: Parsley[String], suffix: Parsley[List[Char]]): Parsley[Identifier] =
    pos <**> (prefix, suffix).map((p, s) => Identifier(p + s.mkString))
}

object IntegerLiter {
  def apply(option: Parsley[Option[IntegerSign]], digits: Parsley[List[Digit]]): Parsley[IntegerLiter] =
    pos <**> (option, digits).map(IntegerLiter(_, _))
}

object PairLiter {
  def apply(string: Parsley[String]): Parsley[PairLiter] = pos <**> string.map(_ => PairLiter())
}

object StringLiter {
  def apply(chars: Parsley[List[DefaultCharacter]]): Parsley[StringLiter] =
    pos <**> chars.map(dcs => StringLiter(dcs.mkString))
}

object UnaryOperatorApplication {
  def apply(operator: Parsley[UnaryOperator], expression: Parsley[Expression]): Parsley[UnaryOperatorApplication] =
    pos <**> (operator, expression).map(UnaryOperatorApplication(_, _))
}

object FunctionCall {
  def apply(identifier: Parsley[Identifier], option: Parsley[Option[ArgumentList]]): Parsley[FunctionCall] =
    pos <**> (identifier, option).map(FunctionCall(_, _))
}

object NewPair {
  def apply(expr1: Parsley[Expression], expr2: Parsley[Expression]): Parsley[NewPair] =
    pos <**> (expr1, expr2).map(NewPair(_, _))
}

object PairElement {
  def apply(expression: Parsley[Expression], isFirst: Boolean): Parsley[PairElement] =
    pos <**> expression.map(PairElement(_, isFirst))
}

object ArgumentList {
  def apply(expr: Parsley[Expression], exprs: Parsley[List[Expression]]): Parsley[ArgumentList] =
    pos <**> (expr, exprs).map((e, es) => ArgumentList(e :: es))
}
