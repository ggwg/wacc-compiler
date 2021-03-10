package com.wacc

import scala.::
import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

class SymbolTable(parentSymbolTable: SymbolTable, isFunctionSymbolTable: Boolean, functionReturnType: Type) {
  // default parameter to null
  def this() = this(null, false, VoidType())

  def this(parentSymbolTable: SymbolTable) = this(parentSymbolTable, false, VoidType())

  val isFunction: Boolean = isFunctionSymbolTable

  val returnType: Type = functionReturnType

  var parent: Option[SymbolTable] = Option(parentSymbolTable)
  /* The symbol table contains a mapping from the name of the variable to a tuple
     containing its type and corresponding AST node. */
  var dictionary: mutable.Map[String, (Type, ASTNode)] = mutable.Map[String, (Type, ASTNode)]()

  var functionDic: mutable.Map[String, List[FunctionType]] = mutable.Map.empty

  /* Adds function to the symbol table. Returns boolean success code.
     Returns false if overloaded function is already defined */
  def addFunction(functionName: String, t: Type): Boolean = {
    /* If the type specified is not a function type, do not add it to the function dictionary */
    t match {
      case functionType @ FunctionType(returnType, parameters) =>
        /* TODO: Refactor this to use list buffer - increased efficiency. */
        functionDic.get(functionName) match {
          case Some(values) =>
            // Check first that the function type is no already the dictionary:
            for (value <- values) {
              if (FunctionType(AnyType(), parameters).unifies(value)) {
                return false
              }
            }
            functionDic += (functionName -> List.concat(values, List(functionType)))
          case None =>
            functionDic += (functionName -> List(functionType))
        }
      case _ => ()
    }
    return true
  }

  /* Looks up all the symbol tables */
  def lookupAllFunction(funcName: String, functionType: FunctionType): Type = {
    var current = Option(this)
    while (current.isDefined) {
      current.get.functionDic.get(funcName) match {
        case Some(expectedFunctionTypes) =>
          // TODO: Refactor to use "Reduce"
          for (expectedFunctionType <- expectedFunctionTypes) {
            if (functionType.unifies(expectedFunctionType)) {
              return expectedFunctionType
            }
          }
          current = current.get.parent
        case None =>
          current = current.get.parent
      }
    }
    /* If no function is found, return voidType to indicate failure */
    VoidType()
  }

  /* Add a variable, along with it's type and corresponding AST node, to the symbol table */
  def add(varName: String, varType: Type, varObj: ASTNode): Unit = {
    dictionary += (varName -> (varType, varObj))
    // dictionary.updated(varName, (varType, varObj))
  }

  // TODO: Refactor null to use Option[]
  // Returns Type object else empty Option if name not in dict
  def lookup(name: String): Option[(Type, ASTNode)] = {
    dictionary.get(name)
  }

  /* Looks up all the symbol tables */
  def lookupAll(varName: String): Option[(Type, ASTNode)] = {
    var current = Option(this)
    while (current.isDefined) {
      val varObj = current.get.lookup(varName)
      if (varObj.isDefined) {
        return varObj
      }
      current = current.get.parent
    }
    None
  }

  def isInsideFunctionSymbolTable(): Boolean = {
    var current = Option(this)
    while (current.isDefined) {
      if (current.get.isFunction)
        return true
      current = current.get.parent
    }
    false
  }

  def getReturnType(): Type = {
    var current = Option(this)
    while (current.isDefined) {
      if (current.get.isFunction)
        return current.get.returnType
      current = current.get.parent
    }
    return VoidType()
  }

  def identifierIsFunction(identifier: String): Boolean = {
    val option = lookupAll(identifier)
    if (option.isEmpty) {
      return false
    }
    option.get._2 match {
      case Function(_, _, _, _) => true
      case _                    => false
    }
  }

  override def toString: String = dictionary.toString()
}
