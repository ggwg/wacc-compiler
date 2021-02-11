package com.wacc

import scala.collection.immutable.Map

class SymbolTable(parentSymbolTable: SymbolTable) {
  // default parameter to null
  def this() = this(null)

  var parent: Option[SymbolTable] = Option(parentSymbolTable)
  /* The symbol table contains a mapping from the name of the variable to a tuple
     containing its type and corresponding AST node. */
  var dictionary: Map[String, (Type, ASTNode)] = Map[String, (Type, ASTNode)]()

  /* Add a variable, along with it's type and corresponding AST node, to the symbol table */
  def add(varName: String, varType: Type, varObj: ASTNode): Unit = {
    dictionary.updated(varName, (varType, varObj))
  }

  // TODO: Refactor null to use Option[]
  // Returns Type object else empty Option if name not in dict
  def lookup(name: String): Option[(Type, ASTNode)] = {
    return dictionary.get(name)
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
    return None
  }

  override def toString: String = dictionary.toString()
}
