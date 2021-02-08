package com.wacc

import com.wacc.TypeEnum.TypeEnum

trait ASTNode {
  // Each statement needs a check function for semantic analysis
  def check(symbolTable: SymbolTable): Unit = {
    println("* NO CHECK DEFINED *")
  }

  def getType(symbolTable: SymbolTable): TypeEnum = {
    return TypeEnum.Void
  }
}
