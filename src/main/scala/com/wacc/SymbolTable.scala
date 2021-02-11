package com.wacc

import scala.collection.mutable

class SymbolTable(parentSymbolTable: SymbolTable, isFunctionSymbolTable: Boolean) {
  // default parameter to null
  def this() = this(null, false)

  def this(parentSymbolTable: SymbolTable) = this(parentSymbolTable, false)

  var isFunction = isFunctionSymbolTable

  var parent: Option[SymbolTable] = Option(parentSymbolTable)
  /* The symbol table contains a mapping from the name of the variable to a tuple
     containing its type and corresponding AST node. */
  var dictionary: mutable.Map[String, (Type, ASTNode)] = mutable.Map[String, (Type, ASTNode)]()

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
    }
    false
  }

  override def toString: String = dictionary.toString()
}
