package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type) = this == otherType
  def asPairElemType(): PairElementType
}

sealed trait PairElementType extends ASTNodeVoid

sealed trait BaseType extends Type with PairElementType

/* New types for categorising statements in Semantic Analysis */
case class VoidType() extends Type with PairElementType {
  override def unifies(otherType: Type): Boolean = false

  override def asPairElemType(): PairElementType = VoidType()
}

case class FunctionType(returnType: Type, parameters: Option[List[Type]]) extends Type {
  override def asPairElemType(): PairElementType = VoidType()
}

//// Change to case object - drop the brackets
//case class IntType()(pos: (Int, Int)) extends BaseType {
//  override def toString: String = "int"
//  override def getType(symbolTable: SymbolTable): Type = IntType()
//}
// Change to case object - drop the brackets
case class IntType() extends BaseType {
  override def toString: String = "int"
  override def getType(symbolTable: SymbolTable): Type = IntType()
  override def asPairElemType(): PairElementType = IntType()
}

case class BooleanType() extends BaseType {
  override def toString: String = "bool"
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
  override def asPairElemType(): PairElementType = BooleanType()
}

case class CharacterType() extends BaseType {
  override def toString: String = "char"
  override def getType(symbolTable: SymbolTable): Type = CharacterType()
  override def asPairElemType(): PairElementType = CharacterType()
}

case class StringType() extends BaseType {
  override def toString: String = "string"
  override def getType(symbolTable: SymbolTable): Type = StringType()
  override def asPairElemType(): PairElementType = StringType()
}

case class PairDefault() extends PairElementType {
  override def toString: String = "pair"
  // override def getType(symbolTable: SymbolTable): Type = VoidType()
}

case class PairType(elementType1: PairElementType, elementType2: PairElementType) extends Type {
  override def toString: String =
    "pair(" + elementType1.toString + ", " + elementType2.toString + ")"
  override def getType(symbolTable: SymbolTable): Type =
    PairType(elementType1, elementType2)
  override def asPairElemType(): PairElementType = PairDefault()
}

case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = "type[" + arrayType.toString + "]"

  // TODO - define check

  override def getType(symbolTable: SymbolTable): Type = ArrayType(arrayType)

  override def asPairElemType(): PairElementType = ArrayType(arrayType)
}

object BaseType {
  val types = List("int", "bool", "char", "string")
  def apply(typeString: Parsley[String]): Parsley[BaseType] = typeString.map {
    case "int"    => IntType()
    case "bool"   => BooleanType()
    case "char"   => CharacterType()
    case "string" => StringType()
  }
}

object PairDefault {
  def apply(string: Parsley[String]): Parsley[PairDefault] = string.map(_ => PairDefault())
}

object PairType {
  def apply(type1: Parsley[PairElementType], type2: Parsley[PairElementType]): Parsley[PairType] =
    (type1, type2).map(PairType(_, _))
}

object ArrayType {
  def apply(arrayType: Parsley[Type]): Parsley[ArrayType] = arrayType.map(ArrayType(_))
}
