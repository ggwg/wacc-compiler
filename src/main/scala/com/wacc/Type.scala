package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type): Boolean
  override def getType(symbolTable: SymbolTable): Type = this
}

sealed trait PairElementType extends Type
sealed trait BaseType extends PairElementType {
  override def unifies(otherType: Type): Boolean = this == otherType
}

/* ✅ Done */
case class PairDefault() extends PairElementType {
  override def toString: String = "pair"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(_, _) | PairDefault() | NullType() => true
    case _                                           => false
  }
}

/* ✅ Done */
case class PairType(fstType: PairElementType, sndType: PairElementType) extends Type {
  override def toString: String = "pair(" + fstType.toString + ", " + sndType.toString + ")"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(fst, snd)         => fstType.unifies(fst) && sndType.unifies(snd)
    case PairDefault() | NullType() => true
    case _                          => false
  }
}

/* ✅ Done */
case class NullType() extends Type {
  override def toString: String = "null"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(_, _) | NullType() | PairDefault() => true
    case _                                           => false
  }
}

/* ✅ Done */
case class EmptyType() extends PairElementType {
  override def toString: String = "[]"
  override def unifies(otherType: Type): Boolean = otherType match {
    case EmptyType() | ArrayType(_) => true
    case _                          => false
  }
}

/* ✅ Done */
case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = arrayType.toString + "[]"
  override def unifies(otherType: Type): Boolean = otherType match {
    case EmptyType()      => true
    case ArrayType(other) => arrayType.unifies(other)
    case _                => false
  }
}

/* ✅ Done - New type for categorising statements in semantic analysis */
case class VoidType() extends PairElementType {
  override def toString: String = "void"
  override def unifies(otherType: Type): Boolean = false
}

/* ✅ Done */
case class FunctionType(returnType: Type, parameters: Option[List[Type]]) extends Type {
  override def toString: String = "(" +
    (parameters match {
      case Some(list: List[Type]) => list.map(_.toString).mkString(", ")
      case None                   => ""
    }) + ") => " + returnType
  override def unifies(otherType: Type): Boolean = otherType match {
    case FunctionType(ret, params) => returnType.unifies(ret) && Type.unifiesParameters(params, parameters)
    case _                         => false
  }
}

/* ✅ Done */
case class IntType() extends BaseType {
  override def toString: String = "int"
}

/* ✅ Done */
case class BooleanType() extends BaseType {
  override def toString: String = "bool"
}

/* ✅ Done */
case class CharacterType() extends BaseType {
  override def toString: String = "char"
}

/* ✅ Done */
case class StringType() extends BaseType {
  override def toString: String = "string"
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
object Type {
  def unifiesParameters(mList1: Option[List[Type]], mList2: Option[List[Type]]): Boolean = {
    val list1 = mList1.getOrElse(List.empty)
    val list2 = mList2.getOrElse(List.empty)

    if (list1.size != list2.size) return false
    val list: List[(Type, Type)] = list1.zip(list2)
    list.forall { case (t1, t2) => t1.unifies(t2) }
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
