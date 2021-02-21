package com.wacc

sealed trait Instruction
sealed trait Operand2
sealed trait AddressAccess
sealed trait Condition
sealed trait Register extends Operand2

case object Register0 extends Register
case object Register1 extends Register
case object Register2 extends Register
case object Register3 extends Register
case object Register4 extends Register
case object Register5 extends Register
case object Register6 extends Register
case object Register7 extends Register
case object Register8 extends Register
case object Register10 extends Register
case object Register11 extends Register
case class ImmediateValue(value: Int) extends Operand2

case class ImmediateLoad(value: Int) extends AddressAccess
case class RegisterLoad(id: Int) extends AddressAccess
case class RegisterOffsetLoad(id: Int, offset: ImmediateValue) extends AddressAccess

case object Equal extends Condition
case object NotEqual extends Condition
case object LessOrEqual extends Condition
case object GreaterOrEqual extends Condition
case object Less extends Condition
case object Greater extends Condition

case class PushLR() extends Instruction
case class PopPC() extends Instruction
case class Add(dest: Register, src: Register, op2: Operand2) extends Instruction
case class Sub(dest: Register, src: Register, op2: Operand2) extends Instruction
case class Mul(dest: Register, multiplier: Register, src: Register) extends Instruction
case class Load(dest: Register, src: AddressAccess) extends Instruction
case class Store(src: Register, dest: AddressAccess) extends Instruction
case class Branch(cond: Option[Condition], label: String) extends Instruction
case class Compare(src: Register, op2: Operand2) extends Instruction
case class Move(dest: Register, op2: Operand2) extends Instruction
