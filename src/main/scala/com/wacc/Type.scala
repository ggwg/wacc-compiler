package com.wacc

sealed trait Type
sealed trait PairElementType extends Type
sealed trait BaseType extends Type with PairElementType

case class IntType() extends BaseType {
  override def toString: String = "int"
}

case class BooleanType() extends BaseType {
  override def toString: String = "bool"
}

case class CharacterType() extends BaseType {
  override def toString: String = "char"
}

case class StringType() extends BaseType {
  override def toString: String = "string"
}

case class PairDefault() extends PairElementType {
  override def toString: String = "pair"
}

case class PairType(
    elementType1: PairElementType,
    elementType2: PairElementType
) extends Type {
  override def toString: String =
    "pair(" + elementType1.toString + ", " + elementType2.toString + ")"
}

case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = "type[" + arrayType.toString + "]"
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
