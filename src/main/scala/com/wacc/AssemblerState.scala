package com.wacc

import scala.collection.immutable

/* This class represents the state of the compiler while it is converting
   the code into assembly */
case class AssemblerState(
  /* How far down is the stack pointer relative to the beginning of the compilation.
     We use this to find where identifiers are stored on the stack. */
  spOffset: Int,
  /* For every identifier in this dictionary we keep the distance between where
     the variable is stored on the stack, and the beginning of the stack, similarly
     to spOffset */
  varDic: immutable.Map[String, Int],
  /* List of available registers to be used when compiling a piece of code */
  freeRegs: immutable.List[Register],
  /* Label ID generator to be used in generating label names for branching */
  nextLabelID: Iterator[Int] = LazyList.iterate(0)(_ + 1).iterator,
  /* How much space on the stack the current code compilation is using for variables.
     Useful when exiting scopes to reset the stack pointer to the original value */
  declaredSize: Int
) {
  /* Returns the register in which an expression's result will be stored */
  def getResultRegister: Register = freeRegs.head

  /* Returns the second register in the list, to be used as needed */
  def getHelperRegister: Register = freeRegs.tail.head

  /* Returns the offset of a given variable name. Since our semantic analysis was successful,
     we are guaranteed to have a mapping for the variable in our varDic */
  def getOffset(ident: String): Int = varDic.getOrElse(ident, 0)

  /* Returns the next free label ID */
  def nextID: Int = nextLabelID.next

  /* Creates a new scope state from the initial state */
  def newScopeState: AssemblerState = this.copy(declaredSize = 0)

  /* When called on a 'scope' state and given its 'parent' state, this function
     adjusts some fields of the scope state to match its parent state. The newly
     returned state can be used to continue to assemble code. */
  def fromScopeToInitialState(state: AssemblerState): AssemblerState =
    this.copy(spOffset = state.spOffset, varDic = state.varDic, declaredSize = state.declaredSize)
}

object AssemblerState {
  /* This is the default initial state with which anything can be executed */
  val initialState: AssemblerState =
    AssemblerState(spOffset = 0, varDic = Map.empty, freeRegs = Register.usableRegisters, declaredSize = 0)
}
