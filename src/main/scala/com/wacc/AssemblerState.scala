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

  val initialState: AssemblerState = AssemblerState(
    spOffset = 0,
    varDic = Map.empty[String, Int],
    freeRegs = List.empty[Register],
    declaredVars = List.empty[String],
    declaredSize = 0
  )
}
