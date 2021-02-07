package com.wacc

sealed trait Type extends ASTNode

sealed trait PairElementType extends Type
sealed trait BaseType extends Type with PairElementType

case class IntType() extends BaseType {
  override def toString: String = "int"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE INT-TYPE CHECK")
    return IntType
  }
}

case class BooleanType() extends BaseType {
  override def toString: String = "bool"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE BOOL-TYPE CHECK")
    return BooleanType
  }
}

case class CharacterType() extends BaseType {
  override def toString: String = "char"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE CHARACTER-TYPE CHECK")
    return CharacterType
  }
}

case class StringType() extends BaseType {
  override def toString: String = "string"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE STRING-TYPE CHECK")
    return StringType
  }
}

case class PairDefault() extends PairElementType {
  override def toString: String = "pair"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE PAIR-DEFAULT CHECK")
    return PairDefault
  }
}

case class PairType(
    elementType1: PairElementType,
    elementType2: PairElementType
) extends Type {
  override def toString: String =
    "pair(" + elementType1.toString + ", " + elementType2.toString + ")"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE PAIR-TYPE CHECK")
    return PairType
  }
}

case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = "type[" + arrayType.toString + "]"

  override def check(symbolTable: SymbolTable): Any = {
    println("GOT INSIDE ARRAY-TYPE CHECK")
    return ArrayType
  }
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
