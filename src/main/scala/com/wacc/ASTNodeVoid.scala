package com.wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/* Represents the general AST node, and some useful methods to
   query information from it. */
trait ASTNode {

  /* Each statement needs a check function for semantic analysis */
  def check(symbolTable: SymbolTable)(implicit errors: mutable.ListBuffer[Error]): Unit = ()

  /* Starting position of the line of code represented in the AST
     Nodes with no need for position tracking are defaulted to (-1, -1) */
  def getPos(): (Int, Int) = (-1, -1)

  /* Returns true if 2 the 2 nodes' types are unifiable */
  def sameTypes(otherASTNode: ASTNode, symbolTable: SymbolTable) =
    this.getType(symbolTable).unifies(otherASTNode.getType(symbolTable))

  /* Returns the actual type of a node */
  def getType(symbolTable: SymbolTable): Type

  // TODO: Remove null
  def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = null
}

/* Create a new trait that extends ASTNode and defined default void return */
trait ASTNodeVoid extends ASTNode {
  override def getType(symbolTable: SymbolTable): Type = VoidType()
}
