package com.wacc

import scala.collection.immutable

case class AssemblerState(
  val spOffset: Int,
  val varDic: immutable.Map[String, Int],
  val freeRegs: immutable.List[Register],
  val nextLabelID: Iterator[Int] = LazyList.iterate(0)(_ + 1).iterator,
  val declaredVars: immutable.List[String],
  val declaredSize: Int
) {
  def getResultRegister: Register = freeRegs.head

  def getOffset(ident: String): Int = varDic.getOrElse(ident, 0)

  def nextID: Int = nextLabelID.next

  def newScopeState: AssemblerState = this.copy(declaredVars = List.empty, declaredSize = 0)

  def fromScopeToInitialState(state: AssemblerState): AssemblerState = this.copy(
    spOffset = state.spOffset,
    varDic = state.varDic,
    declaredSize = state.declaredSize,
    declaredVars = state.declaredVars
  )
}

object AssemblerState {
  val initialState: AssemblerState =
    AssemblerState(
      spOffset = 0,
      varDic = Map.empty,
      freeRegs = Register.usableRegisters,
      declaredVars = List.empty,
      declaredSize = 0
    )
}
