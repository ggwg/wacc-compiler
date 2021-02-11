package com.wacc

import com.wacc.operator._
import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Expression extends AssignmentRight {}
sealed trait AssignmentRight extends ASTNodeVoid {}
sealed trait AssignmentLeft extends ASTNodeVoid {}

/* TODO:
   - Check that expression is an int between 0-255
   - Type checking for arrays (length) -> if (expression.getType(symbolTable).unifies(ArrayType()))
   - Add support for retrieving position from the AST
   - Remove unnecessary print statements
 */
case class UnaryOperatorApplication(unaryOperator: UnaryOperator, expression: Expression) extends Expression {
  override def toString: String = unaryOperator.toString + expression.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking unary operator application...")
    val expressionType = expression.getType(symbolTable)
    val pos = (39, 15)

    unaryOperator match {
      case Chr() =>
        if (expressionType.unifies(IntType())) expression.check(symbolTable)
        else List(UnaryOperatorError("chr", "int", expressionType.toString, pos))
      case Length() =>
        List.empty
      case Negate() =>
        if (expressionType.unifies(IntType())) expression.check(symbolTable)
        else List(UnaryOperatorError("(-) (i.e. negate)", "int", expressionType.toString, pos))
      case Not() =>
        if (expressionType.unifies(BooleanType())) expression.check(symbolTable)
        else List(UnaryOperatorError("not", "boolean", expressionType.toString, pos))
      case Ord() =>
        if (expressionType.unifies(CharacterType())) expression.check(symbolTable)
        else List(UnaryOperatorError("ord", "char", expressionType.toString, pos))
    }
  }

  override def getType(symbolTable: SymbolTable): Type = unaryOperator match {
    case Chr() => CharacterType()
    case Not() => BooleanType()
    case _     => IntType()
  }
}

/* TODO:
   - Find out what to do for the check (returns List.empty for now)
 */
case class PairLiter() extends Expression {
  override def toString: String = "null"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking pair literal...")
    List.empty
  }
}

/* TODO:
  - Add support for position
  - Try to print the expected function signature in the error message
  - Error in check; the funcType will not return a class FunctionType
 */
case class FunctionCall(name: Identifier, arguments: Option[ArgumentList]) extends AssignmentRight {
  override def toString: String =
    "call " + name + "(" + (arguments match {
      case Some(args) => args.toString
      case None       => ""
    }) + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking function call...")
    val func: Option[(Type, ASTNode)] = symbolTable.lookupAll(name.identifier)
    val pos = (39, 15)

    if (func.isEmpty) {
      List(DefaultError("Function " + name.identifier + " not defined in scope", pos))
    } else if (!func.get._2.isInstanceOf[Function]) { // Pattern match
      // Checking if statement is function
      List(DefaultError(name.identifier + " is not a function", pos))
    } else {
//      val thisFunctionType = FunctionType(name.getType(symbolTable), arguments.map(_.expressions.map(_.getType(symbolTable))))
//      if (!func.get._1.unifies(funcType)) List(DefaultError("Type mismatch in arguments of " + name.identifier, pos))
      List.empty
    }
  }

  override def getType(symbolTable: SymbolTable): Type = {
    symbolTable.lookupAll(name.identifier).getOrElse((VoidType(), null))._1
  }
}

/* TODO:
   - Implement argument list checking
   - Implement the getType for this
 */
case class ArgumentList(expressions: List[Expression]) extends ASTNodeVoid {
  override def toString: String = expressions.map(_.toString).reduce((left, right) => left + ", " + right)

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking argument list...")
    for (expression <- expressions) {
      expression.check(symbolTable)
    }
  }
}

/* TODO:
   - Implement the getType override
   - Also try to provide more information about which index access attempt has the error
 */
case class ArrayElement(name: Identifier, expressions: List[Expression]) extends Expression with AssignmentLeft {
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

      errors
    }
  }
}

/* ✅ Check done */
case class BinaryOperatorApplication(leftOperand: Expression, binaryOperator: BinaryOperator, rightOperand: Expression)
    extends Expression {
  override def toString: String = leftOperand.toString + " " + binaryOperator.toString + " " + rightOperand.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking binary operator application")
    val leftType = leftOperand.getType(symbolTable)
    val rightType = rightOperand.getType(symbolTable)
    val op = binaryOperator.toString
    val pos = (39, 15)

    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() =>
        if (!leftType.unifies(IntType()))
          errors += BinaryOperatorError(op, IntType.toString(), leftType.toString, pos, isLeft = true)
        else if (!rightType.unifies(IntType()))
          errors += BinaryOperatorError(op, IntType.toString(), rightType.toString, pos, isLeft = false)
        else
          leftType.check(symbolTable)
        rightType.check(symbolTable)

      case GreaterThan() | GreaterEqualThan() | SmallerThan() | SmallerEqualThan() =>
        val expected = IntType.toString() + " or " + CharacterType.toString()

        if (!(leftType.unifies(IntType()) || leftType.unifies(CharacterType())))
          errors += BinaryOperatorError(op, expected, leftType.toString, pos, isLeft = true)
        else if (!(rightType.unifies(IntType()) || rightType.unifies(CharacterType())))
          errors += BinaryOperatorError(op, expected, rightType.toString, pos, isLeft = false)
        else
          leftType.check(symbolTable)
        rightType.check(symbolTable)

      case Equals() | NotEquals() =>
        if (!leftType.unifies(rightType))
          errors += DefaultError(
            "Cannot compare " + leftType.toString + " and " + rightType.toString + " types " +
              " in " + this.toString,
            pos
          )
        else
          leftType.check(symbolTable)
        rightType.check(symbolTable)

      case And() | Or() =>
        if (!leftOperand.getType(symbolTable).unifies(BooleanType()))
          errors += BinaryOperatorError(op, BooleanType.toString(), leftType.toString, pos, isLeft = true)
        else if (!rightOperand.getType(symbolTable).unifies(BooleanType()))
          errors += BinaryOperatorError(op, BooleanType.toString(), rightType.toString, pos, isLeft = false)
        else
          leftType.check(symbolTable)
        check(symbolTable)
    }
  }

  override def getType(symbolTable: SymbolTable): Type =
    binaryOperator match {
      case Add() | Divide() | Modulo() | Multiply() | Subtract() => IntType()
      case _                                                     => BooleanType()
    }
}

/* ✅ Check done */
case class BooleanLiter(boolean: Boolean) extends Expression {
  override def toString: String = boolean.toString

  override def getType(symbolTable: SymbolTable): Type = BooleanType()
}

/* ✅ Check done */
case class CharacterLiter(char: Char) extends Expression {
  override def toString: String = "'" + char + "'"

  override def getType(symbolTable: SymbolTable): Type = CharacterType()
}

/* ✅ Check done */
case class Identifier(identifier: String) extends Expression with AssignmentLeft {
  override def toString: String = identifier

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking identifier...")
    val pos = (39, 15)
    if (getType(symbolTable).unifies(VoidType())) List(DefaultError("Undefined identifier \"" + identifier + "\"", pos))
    else List.empty
  }

  override def getType(symbolTable: SymbolTable): Type =
    symbolTable.lookupAll(identifier).getOrElse((VoidType(), null))._1
}

/* ✅ Check done */
case class IntegerLiter(sign: Option[IntegerSign], digits: List[Digit]) extends Expression {
  override def toString: String = (sign match {
    case None       => ""
    case Some(sign) => sign.toString
  }) + digits.mkString

  override def getType(symbolTable: SymbolTable): Type = IntType()
}

/* ✅ Check done */
case class StringLiter(string: String) extends Expression {
  override def toString: String = "\"" + string + "\""
  override def getType(symbolTable: SymbolTable): Type = StringType()
}

/* ✅ Check done */
case class ArrayLiter(expressions: List[Expression]) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduceOption((left, right) => left + ", " + right)
    .getOrElse("") + "]"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit =
    for (expression <- expressions) {
      expression.check(symbolTable)
    }

  override def getType(symbolTable: SymbolTable): Type = {
    if (expressions.isEmpty) {
      ArrayType(VoidType())
    } else {
      ArrayType(expressions.head.getType(symbolTable))
    }
  }
}

/* ✅ Check done */
case class NewPair(first: Expression, second: Expression) extends AssignmentRight {
  override def toString: String = "newpair(" + first.toString + ", " + second.toString + ")"

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking newpair...")
    first.check(symbolTable)
    second.check(symbolTable)
  }

  override def getType(symbolTable: SymbolTable): Type = {
    val fstType = first.getType(symbolTable)
    val sndType = second.getType(symbolTable)

    if (!fstType.isInstanceOf[PairElementType] || !sndType.isInstanceOf[PairElementType]) VoidType()
    else PairType(fstType.asInstanceOf[PairElementType], sndType.asInstanceOf[PairElementType])
  }
}

/* ✅ Check done */
case class PairElement(expression: Expression, isFirst: Boolean) extends AssignmentRight with AssignmentLeft {
  override def toString: String = (if (isFirst) "fst " else "snd ") + expression.toString

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    println(">>> Checking pair element...")
    expression.check(symbolTable)
  }

  override def getType(symbolTable: SymbolTable): Type = {
    expression.getType(symbolTable)
  }
}

/*  -----------------------------  Class objects  -----------------------------  */

object ArrayElement {
  val build: (Identifier, List[Expression]) => ArrayElement = ArrayElement(_, _)

  def apply(ident: Parsley[Identifier], exprs: Parsley[List[Expression]]): Parsley[ArrayElement] =
    (ident, exprs).map(ArrayElement(_, _))
}

object ArrayLiter {
  val build: Option[(Expression, List[Expression])] => ArrayLiter = {
    case None          => ArrayLiter(List())
    case Some((e, es)) => ArrayLiter(e :: es)
  }

  def apply(option: Parsley[Option[(Expression, List[Expression])]]): Parsley[ArrayLiter] =
    option.map {
      case None          => ArrayLiter(List())
      case Some((e, es)) => ArrayLiter(e :: es)
    }
}

object BinaryOperatorApplication {
  val build: (Expression, BinaryOperator, Expression) => BinaryOperatorApplication = (e1, op, e2) =>
    BinaryOperatorApplication(e1, op, e2)

  def apply(
    expr1: Parsley[Expression],
    operator: Parsley[BinaryOperator],
    expr2: Parsley[Expression]
  ): Parsley[BinaryOperatorApplication] =
    (expr1, operator, expr2).map(BinaryOperatorApplication(_, _, _))
}

object BooleanLiter {
  val build: String => BooleanLiter = bool => BooleanLiter(bool.equals("true"))

  def apply(bool: Parsley[String]): Parsley[BooleanLiter] = bool.map(b => BooleanLiter(b.equals("true")))
}

object CharacterLiter {
  val build: DefaultCharacter => CharacterLiter = chr => CharacterLiter(chr.char)

  def apply(chr: Parsley[DefaultCharacter]): Parsley[CharacterLiter] = chr.map(c => CharacterLiter(c.char))
}

object Identifier {
  val buildKeywordPrefix: (String, List[Char]) => Identifier = (prefix, suffix) => Identifier(prefix + suffix.mkString)
  val build: (Char, List[Char]) => Identifier = (letter, letters) => Identifier((letter :: letters).mkString)

  def apply(prefix: Parsley[String], suffix: Parsley[List[Char]]): Parsley[Identifier] =
    (prefix, suffix).map((p, s) => Identifier(p + s.mkString))
}

object IntegerLiter {
  val build: (Option[IntegerSign], List[Digit]) => IntegerLiter = IntegerLiter(_, _)

  def apply(option: Parsley[Option[IntegerSign]], digits: Parsley[List[Digit]]): Parsley[IntegerLiter] =
    (option, digits).map(IntegerLiter(_, _))
}

object PairLiter {
  val build: String => PairLiter = _ => PairLiter()

  def apply(string: Parsley[String]): Parsley[PairLiter] = string.map(_ => PairLiter())
}

object StringLiter {
  val build: List[DefaultCharacter] => StringLiter = dcs => StringLiter(dcs.mkString)

  def apply(chars: Parsley[List[DefaultCharacter]]): Parsley[StringLiter] = chars.map(dcs => StringLiter(dcs.mkString))
}

object UnaryOperatorApplication {
  val build: (UnaryOperator, Expression) => UnaryOperatorApplication = UnaryOperatorApplication(_, _)

  def apply(operator: Parsley[UnaryOperator], expression: Parsley[Expression]): Parsley[UnaryOperatorApplication] =
    (operator, expression).map(UnaryOperatorApplication(_, _))
}

object FunctionCall {
  val build: (Identifier, Option[ArgumentList]) => FunctionCall = FunctionCall(_, _)

  def apply(identifier: Parsley[Identifier], option: Parsley[Option[ArgumentList]]): Parsley[FunctionCall] =
    (identifier, option).map(FunctionCall(_, _))
}

object NewPair {
  val build: (Expression, Expression) => NewPair = NewPair(_, _)

  def apply(expr1: Parsley[Expression], expr2: Parsley[Expression]): Parsley[NewPair] =
    (expr1, expr2).map(NewPair(_, _))
}

object PairElement {
  val build: (Expression, Boolean) => PairElement = PairElement(_, _)

  def apply(expression: Parsley[Expression], isFirst: Boolean): Parsley[PairElement] =
    expression.map(PairElement(_, isFirst))
}

object ArgumentList {
  val build: (Expression, List[Expression]) => ArgumentList = (e, es) => ArgumentList(e :: es)

  def apply(expr: Parsley[Expression], exprs: Parsley[List[Expression]]): Parsley[ArgumentList] =
    (expr, exprs).map((e, es) => ArgumentList(e :: es))
}
