package com.wacc

trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable): Unit = println("* NO CHECK DEFINED *")
}
