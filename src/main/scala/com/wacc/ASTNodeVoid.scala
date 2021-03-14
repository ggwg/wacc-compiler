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

  // TODO: Remove null; Maybe add flags in the state so that we add helper functions
  // TODO: at the end of the program
  /* Given a compilation state, this function compiles the current node and stores the list of
     generated instructions in the instructions list. Returns the new state after the entire code was
     compiled
     The classes that need to be compiled are located in the Expression, Statement and Program files.

     When compiling an expression, the result of the expression is put in the first register of the
     free registers list, and the return state will contain all the other remaining registers. No other
     assembler state fields are modified. A precondition of the function is that the state MUST have
     at least TWO free registers.

     When compiling a statement we generate the required instructions and return the new state in which
     many fields may have been modified.

     When compiling the program, we generate 'standalone' assembly codes for every function (i.e. different
     new states each time), and then compile the body with a fresh initial state.

     When compiling a function (paired with a function call), we set up each function argument on
     the stack, from left to right (e.g. for the function int f(int x, int y), in order to call
     f(1, 2) we push 1 on the stack, followed by 2
   */
  def compile(state: AssemblerState)(implicit instructions: ListBuffer[Instruction]): AssemblerState = null
}

/* Create a new trait that extends ASTNode and defined default void return */
trait ASTNodeVoid extends ASTNode {
  override def getType(symbolTable: SymbolTable): Type = NotAType()

  /* Returns a new statement in which all nodes that were impossible to be reached were removed */
  def removeUnreachableStatements(): Any = this
}
