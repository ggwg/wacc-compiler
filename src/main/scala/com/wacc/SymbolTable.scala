package com.wacc

import scala.collection.mutable

class SymbolTable(_parent: SymbolTable) {
  // default parameter to null
  def this() = this(null)

  var parent: Option[SymbolTable] = Option(_parent)
  /* The symbol table contains a mapping from the name of the variable to a tuple
     containing its type and corresponding AST node. */
  var dictionary = new mutable.HashMap[String, (Any, ASTNode)]()

  /* Add a variable, along with it's type and corresponding AST node, to the symbol table */
  def add(varName: String, varType: Any, varObj: ASTNode) : Unit = {
    dictionary(varName) = (varType, varObj)
  }

  // TODO: Refactor null to use Option[]
  // Returns Type object else empty Option if name not in dict
  def lookup(name: String) : Option[(Any, ASTNode)] = {
    return dictionary.get(name)
  }

  /* Looks up all the symbol tables */
  def lookupAll(varName: String) : Option[(Any, ASTNode)] = {
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
