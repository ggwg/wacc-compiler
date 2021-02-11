package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type) = this == otherType
}

sealed trait PairElementType extends Type
sealed trait BaseType extends Type with PairElementType

/* New types for categorising statements in Semantic Analysis */
case class VoidType() extends Type {
  override def unifies(otherType: Type): Boolean = false
}

case class FunctionType(returnType: Type, parameters: Option[List[Type]]) extends Type

//// Change to case object - drop the brackets
//case class IntType()(pos: (Int, Int)) extends BaseType {
//  override def toString: String = "int"
//  override def getType(symbolTable: SymbolTable): Type = IntType()
//}
// Change to case object - drop the brackets
case class IntType() extends BaseType {
  override def toString: String = "int"
  override def getType(symbolTable: SymbolTable): Type = IntType()
}

case class BooleanType() extends BaseType {
  override def toString: String = "bool"
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
}

case class CharacterType() extends BaseType {
  override def toString: String = "char"

  override def getType(symbolTable: SymbolTable): Type = CharacterType()
}

case class StringType() extends BaseType {
  override def toString: String = "string"

  override def getType(symbolTable: SymbolTable): Type = {
    println("GOT INSIDE STRING-TYPE")
    StringType()
  }
}

case class PairDefault() extends PairElementType {
  override def toString: String = "pair"

  override def getType(symbolTable: SymbolTable): Type = PairDefault()
}

case class PairType(elementType1: PairElementType, elementType2: PairElementType) extends PairElementType {
  override def toString: String =
    "pair(" + elementType1.toString + ", " + elementType2.toString + ")"
  override def getType(symbolTable: SymbolTable): Type =
    PairType(elementType1, elementType2)
}

case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = "type[" + arrayType.toString + "]"

  // TODO - define check

  override def getType(symbolTable: SymbolTable): Type = ArrayType(arrayType)
}

object BaseType {
  val types = List("int", "bool", "char", "string")

  val build: (String => BaseType) = (typeString: String) =>
    typeString match {
      case "int"    => IntType()
      case "bool"   => BooleanType()
      case "char"   => CharacterType()
      case "string" => StringType()
    }

  def apply(typeString: Parsley[String]): Parsley[BaseType] = typeString.map {
    case "int"    => IntType()
    case "bool"   => BooleanType()
    case "char"   => CharacterType()
    case "string" => StringType()
  }
}

object PairDefault {
  val build: String => PairDefault = _ => PairDefault()

  def apply(string: Parsley[String]): Parsley[PairDefault] = string.map(_ => PairDefault())
}

object PairType {
  val build: (PairElementType, PairElementType) => PairType = PairType(_, _)

  def apply(type1: Parsley[PairElementType], type2: Parsley[PairElementType]): Parsley[PairType] =
    (type1, type2).map(PairType(_, _))
}

object ArrayType {
  val build: Type => ArrayType = ArrayType(_)

  def apply(arrayType: Parsley[Type]): Parsley[ArrayType] = arrayType.map(ArrayType(_))
}
