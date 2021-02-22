package com.wacc

import scala.collection.immutable

case class AssemblerState(
  val varDic: immutable.Map[String, Int],
  val freeRegs: immutable.List[Register],
  val declaredVars: immutable.List[String],
  val declaredSize: Int
)
