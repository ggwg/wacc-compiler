package com.wacc

trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable): List[Error] = List.empty
  def getType(symbolTable: SymbolTable): Type
}

// create a new trait that extends ASTNode and defined default void return
trait ASTNodeVoid extends ASTNode {
//  override def check(symbolTable: SymbolTable): Unit = {
//    println("* NO CHECK DEFINED FOR THIS NODE *")
//  }
  override def getType(symbolTable: SymbolTable): Type = VoidType()
}
