package com.wacc


trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable): Unit = {
    println("* NO CHECK DEFINED *")
  }

  def getType(symbolTable: SymbolTable): Type = {
    return IntType()
  }
}

// create a new trait that extends ASTNode and defined default void return