package com.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = ()
  def getPos(): (Int, Int) = (-1, -1)
  def sameTypes(otherASTNode: ASTNode, symbolTable: SymbolTable) =
    this.getType(symbolTable).unifies(otherASTNode.getType(symbolTable))
  def getType(symbolTable: SymbolTable): Type
}

// create a new trait that extends ASTNode and defined default void return
trait ASTNodeVoid extends ASTNode {
//  override def check(symbolTable: SymbolTable): Unit = {
//    println("* NO CHECK DEFINED FOR THIS NODE *")
//  }
  override def getType(symbolTable: SymbolTable): Type = VoidType()
}
