package com.wacc

import scala.collection.immutable

case class AssemblerState(
  val varDic: immutable.Map[String, Int],
  val freeRegs: immutable.List[Register],
  val nextLabelID: Iterator[Int] = LazyList.iterate(0)(_ + 1).iterator,
  val declaredVars: immutable.List[String],
  val declaredSize: Int
) {

  def nextID: Int = nextLabelID.next
}
