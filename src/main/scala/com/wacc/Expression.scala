package com.wacc

import com.wacc.operator._
import parsley.Parsley
import parsley.Parsley.pos
import parsley.implicits.{voidImplicitly => _, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Expression extends AssignmentRight {
  /* Returns the size of an expression */
  def getSize: Int = this.getExpressionType.getSize

  /* Returns the type of an expression. If the type depends on identifiers, it was precalculated during the semantic
     check and statically stored in each node */
  def getExpressionType: Type
}

sealed trait AssignmentRight extends ASTNodeVoid

sealed trait AssignmentLeft extends ASTNodeVoid {
  /* Compile the reference of a left assignment and store it in the first free register. */
  def compileReference(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState

  /* Returns the type of the left assignment. If the type couldn't be automatically inferred, it was precalculated
     during the semantic check and statically stored in each node */
  def getLeftType: Type
}

case class IncDec(isPrefix: Boolean, isIncrement: Boolean, operand: Expression)(position: (Int, Int))
    extends Expression {
  val operator: String = if (isIncrement) "++" else "--"

  override def toString: String = {
    if (isPrefix) operator.toString + operand.toString else operand.toString + operator.toString
  }

  override def check(symbolTable: SymbolTable)(implicit errors: ListBuffer[Error]): Unit = {
    val operandType = operand.getType(symbolTable)

    if (!operandType.unifies(IntType())) {
      errors += UnaryOperatorError.expectation(operator, "int", operandType.toString, operand.getPos())
      return
    }
    if (!operand.isInstanceOf[AssignmentLeft]) {
      errors += Error(
        "Expression can not be assigned to; It must be an identifier or an array element access",
        position
      )
      return
    }
    operand.check(symbolTable)
  }

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Evaluate the expression and store it in the first available register */
    val resultReg = state.getResultRegister
    val helperReg = state.getHelperRegister
    var newState = state

    /* Apply the unary operation */
    val diff = if (isIncrement) 1 else -1
    val variable = operand.asInstanceOf[AssignmentLeft]
    val isByte = variable.getLeftType.getSize == 1

    /* Put in result register the reference to the variable */
    newState = variable.compileReference(newState)

    /* Put in helper register the actual value of the variable */
    instructions += LOAD(helperReg, RegisterLoad(resultReg), isByte)

    /* Perform the operation */
    newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
    instructions ++= List(ADDS(helperReg, helperReg, ImmediateNumber(diff)), BLVS(OverflowError.label))
    newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)

    /* Store back the result */
    instructions += STORE(helperReg, RegisterLoad(resultReg), isByte = isByte)
    if (isPrefix) {
      instructions += SUBS(helperReg, helperReg, ImmediateNumber(diff))
    }
    instructions += MOVE(resultReg, helperReg)

    newState
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = IntType()
  override def getExpressionType: Type = IntType()
}

/* Class representing an unary operation (e.g. chr 101) */
case class UnaryOperatorApplication(operator: UnaryOperator, operand: Expression)(position: (Int, Int))
    extends Expression {
  override def toString: String = operator.toString + " " + operand.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Evaluate the expression and store it in the first available register */
    val resultReg = state.getResultRegister
    val helperReg = state.getHelperRegister
    var newState = state

    /* Apply the unary operation */
    operator match {
      case Length() =>
        newState = operand.compile(newState)
        instructions += LOAD(resultReg, RegisterLoad(resultReg))
      case Negate() =>
        newState = operand.compile(newState)
        newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
        instructions ++= List(ReverseSUBS(resultReg, resultReg, ImmediateNumber(0)), BLVS(OverflowError.label))
        newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)
      case Not() =>
        newState = operand.compile(newState)
        instructions += XOR(resultReg, resultReg, ImmediateNumber(1))
      case PrefixInc() | PrefixDec() =>
        val diff = operator match {
          case PrefixInc() => 1
          case PrefixDec() => -1
        }
        val variable = operand.asInstanceOf[AssignmentLeft]
        val isByte = variable.getLeftType.getSize == 1

        /* Put in result register the reference to the variable */
        newState = variable.compileReference(newState)

        /* Put in helper register the actual value of the variable */
        instructions += LOAD(helperReg, RegisterLoad(resultReg), isByte)

        /* Perform the operation */
        newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
        instructions ++= List(ADDS(helperReg, helperReg, ImmediateNumber(diff)), BLVS(OverflowError.label))
        newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)

        /* Store back the result */
        instructions += STORE(helperReg, RegisterLoad(resultReg), isByte = isByte)
        instructions += SUBS(helperReg, helperReg, ImmediateNumber(diff))
        instructions += MOVE(resultReg, helperReg)
      case _ => ()
    }

    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Get the operand type */
    val operandType = operand.getType(symbolTable)

    /* Error generation process */
    operator match {
      /* Chr accepts an integer parameter */
      case Chr() =>
        if (!operandType.unifies(IntType())) {
          errors += UnaryOperatorError.expectation("chr", "int", operandType.toString, operand.getPos())
          return
        }
      /* Negate accepts an integer parameter */
      case Negate() =>
        if (!operandType.unifies(IntType())) {
          errors += UnaryOperatorError.expectation("(-) (i.e. negate)", "int", operandType.toString, operand.getPos())
          return
        }
      /* Not accepts a boolean parameter */
      case Not() =>
        if (!operandType.unifies(BooleanType())) {
          errors += UnaryOperatorError.expectation("not", "boolean", operandType.toString, operand.getPos())
          return
        }
      /* Ord accepts a character parameter */
      case Ord() =>
        if (!operandType.unifies(CharacterType())) {
          errors += UnaryOperatorError.expectation("ord", "char", operandType.toString, operand.getPos())
          return
        }
      /* Length accepts an array */
      case Length() =>
        operandType match {
          case ArrayType(_) | EmptyType() => ()
          case _ =>
            errors += UnaryOperatorError.expectation("length", "array", operandType.toString, operand.getPos())
            return
        }
      case PrefixInc() | PrefixDec() =>
        if (!operandType.unifies(IntType())) {
          errors += UnaryOperatorError.expectation(operator.toString, "int", operandType.toString, operand.getPos())
          return
        }
        if (!operand.isInstanceOf[AssignmentLeft]) {
          errors += Error(
            "Expression can not be assigned to; It must be an identifier or an array element access",
            position
          )
          return
        }
    }

    /* Check operand correctness */
    operand.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = getExpressionType

  override def getExpressionType: Type = operator match {
    case Chr() => CharacterType()
    case Not() => BooleanType()
    case _     => IntType()
  }
}

/* Represents a function call (e.g. call fun(1)) */
case class FunctionCall(name: Identifier, arguments: Option[ArgumentList])(position: (Int, Int))
    extends AssignmentRight {
  val labelPrefix = "f_"
  var thisFunctionType: Type = NotAType()

  override def toString: String =
    "call " + name + "(" + (arguments match {
      case Some(args) => args.toString
      case None       => ""
    }) + ")"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state
    val resultReg = newState.getResultRegister

    /* Total size of the arguments */
    val declaredSize = arguments.map(_.expressions.map(_.getSize).sum).getOrElse(0)

    if (arguments.isDefined) {
      /* Arguments are compiled if present */
      newState = arguments.get.compile(newState)
    }

    /* For overloaded functions, get the name of the label to be branched to first: */
    val functionLabel = newState.getFunctionLabel(name.identifier, thisFunctionType)
    /* Jump to the function, reset the stack pointer, and move the result */
    instructions ++= List(
      BRANCHLINK(labelPrefix + functionLabel),
      ADD(RegisterSP, RegisterSP, ImmediateNumber(declaredSize)),
      MOVE(resultReg, Register0)
    )

    newState.copy(spOffset = newState.spOffset - declaredSize, freeRegs = newState.freeRegs.tail)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {

    /* Get the function type of the current function call
       Extract the supplied parameter types from the function call */
    val calledParams = {
      arguments match {
        case Some(list: ArgumentList) => Some(list.expressions.map(expression => expression.getType(symbolTable)))
        case None                     => None
      }
    }
    val calledFunctionType = FunctionType(AnyType(), calledParams)
    /* Using this functionType, look it up to see if there is a matching function in the symbol table */
    val foundFunction = symbolTable.lookupAllFunction(name.identifier, calledFunctionType)
    /* If lookUpAllFunction returns true then success. */
    if (!foundFunction.unifies(NotAType())) {
      /* Check correctness of all arguments */
      thisFunctionType = foundFunction
      arguments.foreach(_.check(symbolTable))
    } else {
      /* Invalid function call with an identifier that's not a function */
      errors += FunctionCallError.invalid(name.identifier, name.identifier, getPos())
    }
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    val calledParams = {
      arguments match {
        case Some(list: ArgumentList) => Some(list.expressions.map(expression => expression.getType(symbolTable)))
        case None                     => None
      }
    }
    val calledFunctionType = FunctionType(AnyType(), calledParams)
    symbolTable.lookupAllFunction(name.identifier, calledFunctionType) match {
      case FunctionType(returnType, _) =>
        returnType
      case _ =>
        /* If the function was not found in the function table, then check again to see if it is in the
           regular symbol table (as a function type)
         */
        symbolTable.lookupAll(name.identifier) match {
          case Some(value) =>
            value._1 match {
              case FunctionType(returnType, _) => returnType
              case _ => NotAType()
            }
          case None => NotAType()
        }
    }
  }
}

/* Represents a list of arguments (e.g. (expr1, expr2)) */
case class ArgumentList(expressions: List[Expression]) extends ASTNodeVoid {
  override def toString: String = expressions.map(_.toString).reduce((left, right) => left + ", " + right)

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val resultReg = state.getResultRegister
    var newState = state

    /* Process the arguments */
    for (expr <- expressions) {
      /* Get the size of the argument and compile */
      val size = expr.getSize
      newState = expr.compile(newState)

      /* Store the result on the stack */
      instructions ++= List(
        SUB(RegisterSP, RegisterSP, ImmediateNumber(size)),
        STORE(resultReg, RegisterLoad(RegisterSP), size == 1)
      )

      newState = newState.copy(spOffset = newState.spOffset + size, freeRegs = resultReg :: newState.freeRegs)
    }

    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* Check the correctness of each argument */
    expressions.foreach(_.check(symbolTable))
  }
}

/* Represents an array element access (e.g. ident[5][10]) */
case class ArrayElement(name: Identifier, expressions: List[Expression])(position: (Int, Int))
    extends Expression
    with AssignmentLeft {
  var expressionType: Type = NotAType()

  override def toString: String = name.toString + expressions.map("[" + _.toString + "]").mkString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Compile the reference of the specified index, then retrieve the value pointed by it */
    val arrayReg = state.getResultRegister
    val newState = compileReference(state)
    instructions += LOAD(arrayReg, RegisterLoad(arrayReg), expressionType.getSize == 1)
    newState
  }

  override def compileReference(
    state: AssemblerState
  )(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val arrayReg = state.getResultRegister
    var newState = state.copy(freeRegs = state.freeRegs.tail)

    /* Make arrayReg to point where the array is stored on the stack */
    instructions += ADD(arrayReg, RegisterSP, ImmediateNumber(state.spOffset - state.getOffset(name.identifier)))
    for (i <- expressions.indices) {
      val expr = expressions(i)
      /* Compute the expression and store it in an available register */
      val indexReg = newState.getResultRegister

      if (newState.freeRegs.length == 1) {
        newState = newState.copy(freeRegs = arrayReg :: newState.freeRegs)

        /* Store the array pointer on the stack and compile the expression with both registers. Move the result into the
           index register and restore the array. Then reset the SP to where it originally was */
        instructions += PUSH(arrayReg)
        newState = expr.compile(newState.copy(spOffset = newState.spOffset + 4))
        instructions ++= List(POP(arrayReg), ADD(RegisterSP, RegisterSP, ImmediateNumber(4)))

        /* Mark the index register as in use */
        newState = newState.copy(spOffset = newState.spOffset - 4, freeRegs = newState.freeRegs.tail)
      } else {
        newState = expr.compile(newState)
      }

      /* Move to the array we were pointing at */
      newState = newState.putMessageIfAbsent(ArrayIndexNegativeError.errorMessage)
      newState = newState.putMessageIfAbsent(ArrayIndexBoundsError.errorMessage)
      instructions ++= List(
        LOAD(arrayReg, RegisterLoad(arrayReg)),
        MOVE(Register0, indexReg),
        MOVE(Register1, arrayReg),
        BRANCHLINK(ArrayIndexError.label)
      )
      newState = newState.copy(p_check_array_bounds = true, p_throw_runtime_error = true)

      /* Skip over the array size */
      instructions += ADD(arrayReg, arrayReg, ImmediateNumber(4))

      /* Move to the specified location in the array */
      val shift = if (i == expressions.length - 1 && expressionType.getSize == 1) 0 else 2
      instructions += ADDLSL(arrayReg, arrayReg, indexReg, ImmediateNumber(shift))

      /* Add index register back to the free registers list */
      newState = newState.copy(freeRegs = indexReg :: newState.freeRegs)
    }
    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (expressions.isEmpty) {
      errors += ArrayElementError.missing(name.identifier, getPos())
    } else {
      /* Go through each expression and check if it's of type int */
      expressions.foreach(expression => {
        val getExpressionType = expression.getType(symbolTable)
        if (!getExpressionType.unifies(IntType()))
          errors += ArrayElementError.expectation(
            name.identifier,
            IntType.toString(),
            getExpressionType.toString,
            getPos()
          )
      })

      /* Check correctness of each expression */
      expressions.foreach(_.check(symbolTable))
    }

    expressionType = getType(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    var identType = name.getType(symbolTable)

    /* Strip the [] until we find the array's type (e.g. int[][][] a, so a[1] has type int[][] */
    for (_ <- 1 to expressions.length)
      identType match {
        case ArrayType(arrayType) => identType = arrayType
        case _                    => return NotAType()
      }
    identType
  }

  override def getExpressionType: Type = expressionType

  override def getLeftType: Type = expressionType
}

/* Represents a binary operation (e.g. 1 + 2) */
case class BinaryOperatorApplication(leftOperand: Expression, operator: BinaryOperator, rightOperand: Expression)(
  position: (Int, Int)
) extends Expression {
  override def toString: String = leftOperand.toString + " " + operator.toString + " " + rightOperand.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Registers holding the expression results */
    val resultReg = state.getResultRegister
    val firstOp = state.getResultRegister
    val secondOp = state.getHelperRegister

    /* Compute the first operand */
    var newState = leftOperand.compile(state)

    /* Compute the second operand */
    if (newState.freeRegs.length == 1) {
      newState = newState.copy(freeRegs = firstOp :: newState.freeRegs)

      /* Store the array pointer on the stack, compile the expression with both registers, and move the result into the
         second operand and restore the first operand */
      instructions += PUSH(firstOp)
      newState = rightOperand.compile(newState.copy(spOffset = newState.spOffset + 4))
      instructions ++= List(MOVE(secondOp, firstOp), POP(firstOp))

      /* Mark the second operand register as unavailable */
      newState = newState.copy(spOffset = newState.spOffset - 4, freeRegs = newState.freeRegs.tail)
    } else {
      newState = rightOperand.compile(newState)
    }

    /* Apply the specified operation */
    operator match {
      /* Integer operations */
      case Add() =>
        newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
        instructions ++= List(ADDS(resultReg, firstOp, secondOp), BLVS(OverflowError.label))
        newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)

      case Subtract() =>
        newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
        instructions ++= List(SUBS(resultReg, firstOp, secondOp), BLVS(OverflowError.label))
        newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)

      case Multiply() =>
        val tempResultReg = newState.getResultRegister
        newState = newState.putMessageIfAbsent(OverflowError.errorMessage)
        instructions ++= List(
          SMULL(resultReg, tempResultReg, firstOp, secondOp),
          COMPAREASR(tempResultReg, resultReg),
          BLNE(OverflowError.label)
        )
        newState = newState.copy(p_throw_overflow_error = true, p_throw_runtime_error = true)

      case Divide() =>
        newState = newState.putMessageIfAbsent(DivideByZeroError.errorMessage)
        instructions ++= List(
          MOVE(Register0, firstOp),
          MOVE(Register1, secondOp),
          BRANCHLINK(DivideByZeroError.label),
          BRANCHLINK("__aeabi_idiv"),
          MOVE(resultReg, Register0)
        )
        newState = newState.copy(p_check_divide_by_zero = true, p_throw_runtime_error = true)

      case Modulo() =>
        newState = newState.putMessageIfAbsent(DivideByZeroError.errorMessage)
        instructions ++= List(
          MOVE(Register0, firstOp),
          MOVE(Register1, secondOp),
          BRANCHLINK(DivideByZeroError.label),
          BRANCHLINK("__aeabi_idivmod"),
          MOVE(resultReg, Register1)
        )
        newState = newState.copy(p_check_divide_by_zero = true, p_throw_runtime_error = true)

      /* Shift operations */
      case ShiftLeft() =>
        instructions += SHIFTLEFT(resultReg, firstOp, secondOp)

      case ShiftRight() =>
        instructions += SHIFTRIGHT(resultReg, firstOp, secondOp)
      /* Logical operations */
      case And() | BitwiseAnd() =>
        instructions += AND(resultReg, firstOp, secondOp)
      case Or() | BitwiseOr() =>
        instructions += OR(resultReg, firstOp, secondOp)

      /* Comparison operations */
      case Equals() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(EQ)),
          MOVE(resultReg, ImmediateNumber(0), Some(NE))
        )
      case NotEquals() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(NE)),
          MOVE(resultReg, ImmediateNumber(0), Some(EQ))
        )

      case GreaterThan() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(GT)),
          MOVE(resultReg, ImmediateNumber(0), Some(LE))
        )
      case SmallerEqualThan() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(LE)),
          MOVE(resultReg, ImmediateNumber(0), Some(GT))
        )

      case SmallerThan() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(LT)),
          MOVE(resultReg, ImmediateNumber(0), Some(GE))
        )
      case GreaterEqualThan() =>
        instructions ++= List(
          COMPARE(firstOp, secondOp),
          MOVE(resultReg, ImmediateNumber(1), Some(GE)),
          MOVE(resultReg, ImmediateNumber(0), Some(LT))
        )
    }

    /* Mark the second operand register as free */
    newState.copy(freeRegs = secondOp :: newState.freeRegs)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    val left = "left"
    val right = "right"

    /* Extract the operand types */
    val leftType = leftOperand.getType(symbolTable)
    val rightType = rightOperand.getType(symbolTable)
    val op = operator.toString

    /* Error generation process */
    operator match {
      /* Integer operations expect integer parameters */
      case Add() | Divide() | Modulo() | Multiply() | Subtract() | BitwiseAnd() | BitwiseOr() | ShiftLeft() |
          ShiftRight() =>
        if (!leftType.unifies(IntType())) {
          errors += BinaryOperatorError.expectation(op, IntType.toString(), leftType.toString, getPos(), left)
          return
        } else if (!rightType.unifies(IntType())) {
          errors += BinaryOperatorError.expectation(op, IntType.toString(), rightType.toString, getPos(), right)
          return
        }
      /* Comparison operations expect integers or characters */
      case GreaterThan() | GreaterEqualThan() | SmallerThan() | SmallerEqualThan() =>
        val expected = IntType.toString() + " or " + CharacterType.toString()

        if (!(leftType.unifies(IntType()) || leftType.unifies(CharacterType()))) {
          errors += BinaryOperatorError.expectation(op, expected, leftType.toString, getPos(), left)
          return
        } else if (!(rightType.unifies(IntType()) || rightType.unifies(CharacterType()))) {
          errors += BinaryOperatorError.expectation(op, expected, rightType.toString, getPos(), right)
          return
        }
      /* Equal operations expect any same types */
      case Equals() | NotEquals() =>
        if (!leftType.unifies(rightType)) {
          errors += BinaryOperatorError.comparison(op, leftType.toString, rightType.toString, getPos())
          return
        }
      /* Boolean operations expect boolean types */
      case And() | Or() =>
        if (!leftType.unifies(BooleanType())) {
          errors += BinaryOperatorError.expectation(op, BooleanType.toString(), leftType.toString, getPos(), left)
          return
        } else if (!rightType.unifies(BooleanType())) {
          errors += BinaryOperatorError.expectation(op, BooleanType.toString(), rightType.toString, getPos(), right)
          return
        }
    }

    /* Check the operands */
    leftOperand.check(symbolTable)
    rightOperand.check(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = getExpressionType

  override def getExpressionType: Type = operator match {
    case Add() | Divide() | Modulo() | Multiply() | Subtract() | BitwiseAnd() | BitwiseOr() | ShiftLeft() |
        ShiftRight() =>
      IntType()
    case _ => BooleanType()
  }
}

/* Represents a bool (true or false) */
case class BooleanLiter(boolean: Boolean)(position: (Int, Int)) extends Expression {
  override def toString: String = boolean.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Move 1 or 0 into the destination register */
    val n = if (boolean) 1 else 0
    instructions += MOVE(state.getResultRegister, ImmediateNumber(n))
    state.copy(freeRegs = state.freeRegs.tail)
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
  override def getExpressionType: Type = BooleanType()
}

/* Represents a character(e.g. 'a')*/
case class CharacterLiter(char: Char)(position: (Int, Int)) extends Expression {
  override def toString: String = "'" + char + "'"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    instructions += MOVE(state.getResultRegister, ImmediateChar(char))
    state.copy(freeRegs = state.freeRegs.tail)
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = CharacterType()
  override def getExpressionType: Type = CharacterType()
}

/* Represents an identifier(e.g. int myIdentifier) */
case class Identifier(identifier: String)(position: (Int, Int)) extends Expression with AssignmentLeft {
  var expressionType: Type = NotAType()

  override def toString: String = identifier

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val resultReg = state.getResultRegister

    /* Find the position of the identifier in the stack and access it*/
    val newState = compileReference(state)
    instructions += LOAD(resultReg, RegisterLoad(resultReg), expressionType.getSize == 1)
    newState
  }

  override def compileReference(
    state: AssemblerState
  )(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Find the position of the identifier in the stack relative to the SP */
    val offset: Int = state.spOffset - state.getOffset(identifier)
    instructions += ADD(state.getResultRegister, RegisterSP, ImmediateNumber(offset))
    state.copy(freeRegs = state.freeRegs.tail)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (getType(symbolTable) == NotAType()) {
      /* Identifier was a void type - indicating that it hasn't been defined yet */
      errors += IdentifierError.undefined(identifier, getPos())
    }

    expressionType = getType(symbolTable)
  }

  override def getType(symbolTable: SymbolTable): Type =
    /* Look up the symbol table hierarchy for the type of the identifier using it's name */
    symbolTable.lookupAll(identifier).getOrElse((NotAType(), null))._1

  override def getExpressionType: Type = expressionType
  override def getLeftType: Type = expressionType
  override def getPos(): (Int, Int) = position
}

/* Represents an integer (e.g. 1234 or -100) */
case class IntegerLiter(sign: Option[IntegerSign], base: Option[Char], digits: List[Digit])(position: (Int, Int))
    extends Expression {
  var amount = 0

  override def toString: String = sign.map(_.toString).getOrElse("") + base.map("0" + _).getOrElse("") + digits.mkString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    instructions += LOAD(state.getResultRegister, ImmediateLoad(amount))
    state.copy(freeRegs = state.freeRegs.tail)
  }

  override def check(symbolTable: SymbolTable)(implicit errors: ListBuffer[Error]): Unit = {
    /* Map the characters to digits */
    val values = digits.map(digit => Digit.valueOf(digit.digit))
    val baseValue = base match {
      case Some('b') => 2
      case Some('o') => 8
      case Some('d') => 10
      case Some('x') => 16
      case None      => 10
    }
    var value: Long = 0

    /* Extract sign value of the number */
    var signValue = 1
    if (sign.nonEmpty && sign.get.sign == '-') signValue = -1

    /* Check that the number does not exceed the bounds */
    for (i <- values) {
      if (i >= baseValue) {
        errors += Error("Integer literal used illegal digits", position)
        return
      }
      value = (value * baseValue) + signValue * i
      if (value > Integer.MAX_VALUE) {
        errors += BoundError.exceed(IntType.toString(), getPos(), isMinimum = false)
        return
      } else if (value < Integer.MIN_VALUE) {
        errors += BoundError.exceed(IntType.toString(), getPos(), isMinimum = true)
        return
      }
    }

    amount = value.toInt
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = IntType()
  override def getExpressionType: Type = IntType()
}

/* Represents a string (e.g. "Hello, World!") */
case class StringLiter(string: String)(position: (Int, Int)) extends Expression {
  override def toString: String = "\"" + string + "\""

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    var newState = state

    /* Check if we already created the message, retrieve the message ID, and load it into the result register */
    newState = newState.putMessageIfAbsent(string)
    val messageID = newState.getMessageID(string)
    instructions += LOAD(newState.getResultRegister, MessageLoad(messageID))

    newState.copy(freeRegs = newState.freeRegs.tail)
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = StringType()
  override def getExpressionType: Type = StringType()
}

/* Represents an array literal(e.g. [1, 2, 3, 4]) */
case class ArrayLiter(expressions: List[Expression])(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "[" + expressions
    .map(_.toString)
    .reduceOption((left, right) => left + ", " + right)
    .getOrElse("") + "]"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val size = if (expressions.isEmpty) 0 else expressions.head.getSize

    /* Allocate memory for the array */
    instructions ++= List(LOAD(Register0, ImmediateLoad(4 + size * expressions.length)), BRANCHLINK("malloc"))

    var newState = state.copy(freeRegs = state.freeRegs.tail)
    val arrayReg: Register = state.getResultRegister
    val valueReg: Register = newState.getResultRegister

    /* Initialize the array size; then free up r0 by moving it in the result register*/
    instructions ++= List(
      MOVE(valueReg, ImmediateNumber(expressions.length)),
      STORE(valueReg, RegisterLoad(Register0)),
      MOVE(arrayReg, Register0)
    )

    /* For each expression, add it to the corresponding place in the array */
    for (index <- expressions.indices) {
      newState = expressions(index).compile(newState)
      instructions += STORE(valueReg, RegisterOffsetLoad(arrayReg, ImmediateNumber(4 + index * size)), size == 1)

      /* Make the value register available for the next expression */
      newState = newState.copy(freeRegs = valueReg :: newState.freeRegs)
    }
    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    if (expressions.nonEmpty) {
      /* Get the type of the first element */
      val arrayElementType = expressions.head.getType(symbolTable)

      /* All other elements must have the same type */
      for (expression <- expressions.tail) {
        val getExpressionType = expression.getType(symbolTable)
        if (!getExpressionType.unifies(arrayElementType)) {
          errors += ArrayLiterError.expectation(
            arrayElementType.toString,
            getExpressionType.toString,
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

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    instructions += LOAD(state.getResultRegister, ImmediateLoad(0))
    state.copy(freeRegs = state.freeRegs.tail)
  }

  override def getPos(): (Int, Int) = position
  override def getType(symbolTable: SymbolTable): Type = NullType()
  override def getExpressionType: Type = NullType()
}

/* Represents creation of a new pair (e.g. newPair(1, "one")) */
case class NewPair(first: Expression, second: Expression)(position: (Int, Int)) extends AssignmentRight {
  override def toString: String = "newpair(" + first.toString + ", " + second.toString + ")"

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Allocate memory for the 2 pointers inside the pair */
    val pairReg: Register = state.getResultRegister
    val valueReg: Register = state.getHelperRegister
    instructions ++= List(LOAD(Register0, ImmediateLoad(8)), BRANCHLINK("malloc"), MOVE(pairReg, Register0))

    /* Evaluate the two expressions */
    var newState = state.copy(freeRegs = state.freeRegs.tail)
    for (offset <- List(0, 4)) {

      /* Evaluate the expression */
      val expr = if (offset == 0) first else second
      newState = expr.compile(newState)

      /* Allocate memory for the pointer */
      val size = expr.getSize
      instructions ++= List(LOAD(Register0, ImmediateLoad(size)), BRANCHLINK("malloc"))

      /* Link the data together */
      instructions ++= List(
        STORE(valueReg, RegisterLoad(Register0), size == 1),
        STORE(Register0, RegisterOffsetLoad(pairReg, ImmediateNumber(offset)))
      )
      newState = newState.copy(freeRegs = valueReg :: newState.freeRegs)
    }
    newState
  }
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
      case NotAType() => return NotAType()
      case _          => ()
    }
    sndType match {
      case NotAType() => return NotAType()
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
      case _                           => NotAType()
    }
  }
}

/* Represents the access of a pair (e.g. fst myPair, snd x) */
case class PairElement(expression: Expression, isFirst: Boolean)(position: (Int, Int))
    extends AssignmentRight
    with AssignmentLeft {
  var pairElementType: Type = NotAType()

  override def toString: String = (if (isFirst) "fst " else "snd ") + expression.toString

  override def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    val resultReg = state.getResultRegister

    /* Find the address of the pair element and access it */
    val newState = compileReference(state)
    instructions += LOAD(resultReg, RegisterLoad(resultReg))
    newState
  }

  override def compileReference(
    state: AssemblerState
  )(implicit instructions: ListBuffer[Instruction]): AssemblerState = {
    /* Evaluate the expression */
    val resultReg = state.getResultRegister
    var newState = expression.compile(state)
    val offset = if (isFirst) 0 else 4

    /* Check for null pointer */
    newState = newState.putMessageIfAbsent(NullDereferenceError.errorMessage)
    instructions += MOVE(Register0, resultReg)
    instructions += BRANCHLINK(NullDereferenceError.label)
    newState = newState.copy(p_check_null_pointer = true, p_throw_runtime_error = true)

    /* Access the first or second pointer */
    instructions += LOAD(resultReg, RegisterOffsetLoad(resultReg, ImmediateNumber(offset)))
    newState
  }

  override def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = {
    /* The expression must be a pair */
    val getExpressionType = expression.getType(symbolTable)
    getExpressionType match {
      case PairType(_, _) => ()
      case _ =>
        errors += PairElementError.expectation("pair", getExpressionType.toString, isFirst, expression.getPos())
    }

    expression.check(symbolTable)

    pairElementType = getType(symbolTable)
  }

  override def getPos(): (Int, Int) = position

  override def getType(symbolTable: SymbolTable): Type = {
    expression.getType(symbolTable) match {
      /* Expression is a pair */
      case PairType(fstType, sndType) =>
        if (isFirst) fstType.getType(symbolTable)
        else sndType.getType(symbolTable)
      /* Otherwise, it is invalid */
      case _ => NotAType()
    }
  }

  override def getLeftType: Type = pairElementType
}

/*  -----------------------------  Class objects  -----------------------------  */

object ArrayElement {
  def apply(identifier: Parsley[Identifier], expressions: Parsley[List[Expression]]): Parsley[ArrayElement] =
    pos <**> (identifier, expressions).map(ArrayElement(_, _))
}

object ArrayLiter {
  def apply(maybeExpressions: Parsley[Option[(Expression, List[Expression])]]): Parsley[ArrayLiter] =
    pos <**> maybeExpressions.map {
      case Some((expression, expressions)) => ArrayLiter(expression :: expressions)
      case None                            => ArrayLiter(List())
    }
}

object BinaryOperatorApplication {
  def apply(
    leftOperand: Parsley[Expression],
    operator: Parsley[BinaryOperator],
    rightOperand: Parsley[Expression]
  ): Parsley[BinaryOperatorApplication] =
    pos <**> (leftOperand, operator, rightOperand).map(BinaryOperatorApplication(_, _, _))
}

object BooleanLiter {
  def apply(bool: Parsley[String]): Parsley[BooleanLiter] = pos <**> bool.map(bool => BooleanLiter(bool.equals("true")))
}

object CharacterLiter {
  def apply(character: Parsley[DefaultCharacter]): Parsley[CharacterLiter] =
    pos <**> character.map(character => CharacterLiter(character.char))
}

object Identifier {
  def apply(prefix: Parsley[String], suffix: Parsley[List[Char]]): Parsley[Identifier] =
    pos <**> (prefix, suffix).map((prefix, suffix) => Identifier(prefix + suffix.mkString))
}

object IntegerLiter {
  def apply(
    sign: Parsley[Option[IntegerSign]],
    base: Parsley[Option[Char]],
    digits: Parsley[List[Digit]]
  ): Parsley[IntegerLiter] =
    pos <**> (sign, base, digits).map(IntegerLiter(_, _, _))
}

object PairLiter {
  def apply(string: Parsley[String]): Parsley[PairLiter] = pos <**> string.map(_ => PairLiter())
}

object StringLiter {
  def apply(characters: Parsley[List[DefaultCharacter]]): Parsley[StringLiter] =
    pos <**> characters.map(character => StringLiter(character.mkString))
}

object UnaryOperatorApplication {
  def apply(operator: Parsley[UnaryOperator], operand: Parsley[Expression]): Parsley[UnaryOperatorApplication] =
    pos <**> (operator, operand).map(UnaryOperatorApplication(_, _))
}

object FunctionCall {
  def apply(name: Parsley[Identifier], arguments: Parsley[Option[ArgumentList]]): Parsley[FunctionCall] =
    pos <**> (name, arguments).map(FunctionCall(_, _))
}

object NewPair {
  def apply(first: Parsley[Expression], second: Parsley[Expression]): Parsley[NewPair] =
    pos <**> (first, second).map(NewPair(_, _))
}

object PairElement {
  def apply(expression: Parsley[Expression], isFirst: Boolean): Parsley[PairElement] =
    pos <**> expression.map(PairElement(_, isFirst))
}

object ArgumentList {
  def apply(expression: Parsley[Expression], expressions: Parsley[List[Expression]]): Parsley[ArgumentList] =
    (expression, expressions).map((e, es) => ArgumentList(e :: es))
}

object IncDec {
  def apply(isPrefix: Boolean, isIncrement: Boolean, expression: Parsley[Expression]): Parsley[IncDec] =
    pos <**> expression.map(IncDec(isPrefix, isIncrement, _))
}
