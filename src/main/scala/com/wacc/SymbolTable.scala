package com.wacc

import scala.collection.mutable

class SymbolTable(enclosingSymbolTable: Option[SymbolTable]) {

  var encSymbolTable: Option[SymbolTable] = enclosingSymbolTable
  var dict = new mutable.HashMap[String, ASTNode]()

  def add(name: String, obj: ASTNode) : Unit = {
    dict(name) = obj
  }

  // TODO: Refactor null to use Option[]
  // Returns Type object else null if name not in dict
  // NOTE: can also use dict.get(), which returns Option[Type]
  def lookup(name: String) : Option[ASTNode] = {
    return dict.get(name)
  }

  def lookupAll(name: String) : Option[ASTNode] = {
    var currST: Option[SymbolTable] = Option(this)
    while (currST.isDefined) {
      var obj: Option[ASTNode] = currST.get.lookup(name)
      if (obj.isDefined) {
        return obj
      }
      currST = currST.get.encSymbolTable
    }
    return None
  }

  override def toString: String = dict.toString()

}
