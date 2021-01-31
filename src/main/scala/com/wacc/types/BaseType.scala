package com.wacc.types

trait BaseType extends Type with PairElementType {}

object BaseType {
  def apply(baseType: String): BaseType = baseType match {
    case "int"    => new IntType()
    case "bool"   => new BooleanType()
    case "char"   => new CharacterType()
    case "string" => new StringType()
  }
}
