package com.wacc

import parsley.Parsley
import parsley.implicits.{voidImplicitly => _, _}

/* This trait represents every type */
sealed trait Type extends ASTNodeVoid {
  def unifies(otherType: Type): Boolean
  override def getType(symbolTable: SymbolTable): Type = this
  def getSize: Int = this match {
    case BooleanType() | CharacterType() => 1
    case _                               => 4
  }
}

/* Void Type for void function calls */
case class VoidType() extends Type {
  override def toString: String = "void"
  override def unifies(otherType: Type): Boolean = this == otherType
}

/* This trait represents types that can appear inside a pair */
sealed trait PairElementType extends Type
sealed trait BaseType extends PairElementType {
  override def unifies(otherType: Type): Boolean = this == otherType
}

/* Represents the type of nested pair declaration (e.g. pair(pair, int) myPair) */
case class PairDefault() extends PairElementType {
  override def toString: String = "pair"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(_, _) | PairDefault() | NullType() => true
    case _                                           => false
  }
}

/* Represents the type of a pair (e.g. pair(int, char); pair(int[][], pair)) */
case class PairType(fstType: PairElementType, sndType: PairElementType) extends Type {
  override def toString: String = "pair(" + fstType.toString + ", " + sndType.toString + ")"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(fst, snd)         => fstType.unifies(fst) && sndType.unifies(snd)
    case PairDefault() | NullType() => true
    case _                          => false
  }
}

/* Represents the null pair (e.g. pair(int, int) = null) */
case class NullType() extends Type {
  override def toString: String = "null"
  override def unifies(otherType: Type): Boolean = otherType match {
    case PairType(_, _) | NullType() | PairDefault() => true
    case _                                           => false
  }
}

/* Represents the type of an array with no elements (e.g. []) */
case class EmptyType() extends PairElementType {
  override def toString: String = "[]"
  override def unifies(otherType: Type): Boolean = otherType match {
    case EmptyType() | ArrayType(_) => true
    case _                          => false
  }
}

/* Represents the type of an array (e.g. int[][]) */
case class ArrayType(arrayType: Type) extends Type with PairElementType {
  override def toString: String = arrayType.toString + "[]"
  override def unifies(otherType: Type): Boolean = otherType match {
    case EmptyType()      => true
    case ArrayType(other) => arrayType.unifies(other)
    case _                => false
  }
}

/* Error type. This type represents errors caught during the semantic analysis. */
case class NotAType() extends PairElementType {
  override def toString: String = "NOT A TYPE"
  override def unifies(otherType: Type): Boolean = false
}

/* AnyType is a type used to help compare 2 function types for function overloading:
   We are not concerned about the return type of the function in this case.
 */
case class AnyType() extends Type {
  override def toString: String = "Any Type (Undefined)"
  override def unifies(otherType: Type): Boolean = true
}

/* This type represents the type of a function (e.g. int fun(int, char)) */
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

/* Integer type */
case class IntType() extends BaseType {
  override def toString: String = "int"
}

/* Boolean type */
case class BooleanType() extends BaseType {
  override def toString: String = "bool"
}

/* Character type */
case class CharacterType() extends BaseType {
  override def toString: String = "char"
}

/* String type */
case class StringType() extends BaseType {
  override def toString: String = "string"
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

object Type {
  def unifiesParameters(mList1: Option[List[Type]], mList2: Option[List[Type]]): Boolean = {
    val list1 = mList1.getOrElse(List.empty)
    val list2 = mList2.getOrElse(List.empty)

    if (list1.size != list2.size) return false
    val list: List[(Type, Type)] = list1.zip(list2)
    list.forall { case (t1, t2) => t1.unifies(t2) }
  }
}

object VoidType {
  def apply(string: Parsley[String]): Parsley[VoidType] = string.map(_ => VoidType())
}

object PairDefault {
  def apply(string: Parsley[String]): Parsley[PairDefault] = string.map(_ => PairDefault())
}

object PairType {
  def apply(fstType: Parsley[PairElementType], sndType: Parsley[PairElementType]): Parsley[PairType] =
    (fstType, sndType).map(PairType(_, _))
}

object ArrayType {
  def apply(arrayType: Parsley[Type]): Parsley[ArrayType] = arrayType.map(ArrayType(_))
}

object FunctionType {
  def apply(ret: Parsley[Type], params: Parsley[Option[List[Type]]]): Parsley[FunctionType] =
    (ret, params).map(FunctionType(_, _))
}
