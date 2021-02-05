package com.wacc

trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable): Any =
    println("* NO CHECK DEFINED *")

}
