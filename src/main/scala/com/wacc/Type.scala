package com.wacc

sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type) = this == otherType
}

sealed trait PairElementType extends Type
sealed trait BaseType extends Type with PairElementType

case class VoidType() extends BaseType

case class IntType() extends BaseType {
  override def toString: String = "int"
  override def getType(symbolTable: SymbolTable): PairElementType = IntType()
}

case class BooleanType() extends BaseType {
  override def toString: String = "bool"
  override def getType(symbolTable: SymbolTable): PairElementType = BooleanType()
}

case class CharacterType() extends BaseType {
  override def toString: String = "char"

  override def getType(symbolTable: SymbolTable): PairElementType = CharacterType()
}

case class StringType() extends BaseType {
  override def toString: String = "string"

  override def getType(symbolTable: SymbolTable): PairElementType = {
    println("GOT INSIDE STRING-TYPE")
    StringType()
  }
}

case class PairDefault() extends PairElementType {
  override def toString: String = "pair"

  override def getType(symbolTable: SymbolTable): PairElementType = PairDefault()
}

case class PairType(
    elementType1: PairElementType,
    elementType2: PairElementType
) extends PairElementType {
  override def toString: String =
    "pair(" + elementType1.toString + ", " + elementType2.toString + ")"
  override def getType(symbolTable: SymbolTable): PairElementType = PairType(elementType1, elementType2)
}

case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = "type[" + arrayType.toString + "]"

  // TODO?
  override def check(symbolTable: SymbolTable): Unit = {
    println("GOT INSIDE ARRAY-TYPE CHECK")
  }

  override def getType(symbolTable: SymbolTable): PairElementType = ArrayType(arrayType)
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
}

object PairDefault {
  val build: String => PairDefault = _ => PairDefault()
}

object PairType {
  val build: (PairElementType, PairElementType) => PairType = PairType(_, _)
}

object ArrayType {
  val build: Type => ArrayType = ArrayType(_)
}
