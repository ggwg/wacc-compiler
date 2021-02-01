package com.wacc

sealed trait Type
sealed trait PairElementType extends Type
sealed trait BaseType extends Type with PairElementType

case class IntType() extends BaseType

case class BooleanType() extends BaseType

case class CharacterType() extends BaseType

case class StringType() extends BaseType

case class PairDefault() extends PairElementType

case class Pair() extends PairElementType

case class PairType(
    val elementType1: PairElementType,
    val elementType2: PairElementType
) extends Type

case class ArrayType(val arrayType: Type) extends Type with PairElementType

object BaseType {
  def apply(baseType: String): BaseType = baseType match {
    case "int"    => new IntType()
    case "bool"   => new BooleanType()
    case "char"   => new CharacterType()
    case "string" => new StringType()
  }
}
