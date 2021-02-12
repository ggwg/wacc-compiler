package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type): Boolean = this == otherType
  def asPairElemType(): PairElementType
}

sealed trait PairElementType extends ASTNodeVoid
sealed trait BaseType extends Type with PairElementType

/* TODO
   - A pair is used in the nested case. It's type should be the type of another pair... */
case class PairDefault() extends PairElementType {
  override def toString: String = "pair"
  // override def getType(symbolTable: SymbolTable): Type = VoidType()
}

/* ✅ Done */
case class PairType(fstType: PairElementType, sndType: PairElementType) extends Type {
  override def toString: String = "pair(" + fstType.toString + ", " + sndType.toString + ")"
  override def getType(symbolTable: SymbolTable): Type = PairType(fstType, sndType)
  override def asPairElemType(): PairElementType = PairDefault()
}

/* ✅ Done */
case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = arrayType.toString + "[]"
  override def getType(symbolTable: SymbolTable): Type = ArrayType(arrayType)
  override def asPairElemType(): PairElementType = ArrayType(arrayType)
}

/* ✅ Done - New type for categorising statements in semantic analysis */
case class VoidType() extends Type with PairElementType {
  override def unifies(otherType: Type): Boolean = false
  override def asPairElemType(): PairElementType = VoidType()
}

/* ✅ Done */
case class FunctionType(returnType: Type, parameters: Option[List[Type]]) extends Type {
  override def asPairElemType(): PairElementType = VoidType()
  override def toString: String = "(" +
    (parameters match {
      case Some(list: List[Type]) => list.map(_.toString).mkString(", ")
      case None                   => ""
    }) + ") => " + returnType
}

/* ✅ Done */
case class IntType() extends BaseType {
  override def toString: String = "int"
  override def getType(symbolTable: SymbolTable): Type = IntType()
  override def asPairElemType(): PairElementType = IntType()
}

/* ✅ Done */
case class BooleanType() extends BaseType {
  override def toString: String = "bool"
  override def getType(symbolTable: SymbolTable): Type = BooleanType()
  override def asPairElemType(): PairElementType = BooleanType()
}

/* ✅ Done */
case class CharacterType() extends BaseType {
  override def toString: String = "char"
  override def getType(symbolTable: SymbolTable): Type = CharacterType()
  override def asPairElemType(): PairElementType = CharacterType()
}

/* ✅ Done */
case class StringType() extends BaseType {
  override def toString: String = "string"
  override def getType(symbolTable: SymbolTable): Type = StringType()
  override def asPairElemType(): PairElementType = StringType()
}

/* ✅ Done */
object BaseType {
  val types = List("int", "bool", "char", "string")
  def apply(typeString: Parsley[String]): Parsley[BaseType] = typeString.map {
    case "int"    => IntType()
    case "bool"   => BooleanType()
    case "char"   => CharacterType()
    case "string" => StringType()
  }
}

/* ✅ Done */
object PairDefault {
  def apply(string: Parsley[String]): Parsley[PairDefault] = string.map(_ => PairDefault())
}

/* ✅ Done */
object PairType {
  def apply(fstType: Parsley[PairElementType], sndType: Parsley[PairElementType]): Parsley[PairType] =
    (fstType, sndType).map(PairType(_, _))
}

/* ✅ Done */
object ArrayType {
  def apply(arrayType: Parsley[Type]): Parsley[ArrayType] = arrayType.map(ArrayType(_))
}
