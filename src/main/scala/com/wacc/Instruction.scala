package com.wacc

sealed trait Instruction
sealed trait Operand2
sealed trait AddressAccess
sealed trait Condition
sealed trait Register extends Operand2

case object Register0 extends Register {
  override def toString: String = "r0"
}

case object Register1 extends Register {
  override def toString: String = "r1"
}

case object Register2 extends Register {
  override def toString: String = "r2"
}

case object Register3 extends Register {
  override def toString: String = "r3"
}

case object Register4 extends Register {
  override def toString: String = "r4"
}

case object Register5 extends Register {
  override def toString: String = "r5"
}

case object Register6 extends Register {
  override def toString: String = "r6"
}

case object Register7 extends Register {
  override def toString: String = "r7"
}
case object Register8 extends Register {
  override def toString: String = "r8"
}

case object Register10 extends Register {
  override def toString: String = "r10"
}

case object Register11 extends Register {
  override def toString: String = "r10"
}

case class ImmediateValue(value: Int) extends Operand2 {
  override def toString: String = "#" + value
}

case class ImmediateLoad(value: Int) extends AddressAccess {
  override def toString: String = "=" + value
}

case class RegisterLoad(reg: Register) extends AddressAccess {
  override def toString: String = s"[$reg]"
}

case class RegisterOffsetLoad(reg: Register, offset: ImmediateValue) extends AddressAccess {
  override def toString: String = s"[$reg, $offset]"
}

case object Equal extends Condition {
  override def toString: String = "EQ"
}

case object NotEqual extends Condition {
  override def toString: String = "NE"
}

case object LessOrEqual extends Condition {
  override def toString: String = "LE"
}

case object GreaterOrEqual extends Condition {
  override def toString: String = "GE"
}

case object Less extends Condition {
  override def toString: String = "LT"
}

case object Greater extends Condition {
  override def toString: String = "GT"
}

case class PushLR() extends Instruction {
  override def toString: String = "PUSH {LR}"
}

case class PopPC() extends Instruction {
  override def toString: String = "POP {PC}"
}

case class Add(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"ADD $dest $src $op2"
}

case class Sub(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"SUB $dest $src $op2"
}

case class Mul(dest: Register, multiplier: Register, src: Register) extends Instruction {
  override def toString: String = s"MUL $dest $multiplier $src"
}

case class Load(dest: Register, src: AddressAccess) extends Instruction {
  override def toString: String = s"LDR $dest $src"
}

case class Store(src: Register, dest: AddressAccess) extends Instruction {
  override def toString: String = s"STR $src $dest"
}

case class Branch(cond: Option[Condition], label: String) extends Instruction {
  override def toString: String = s"B${cond.map(_.toString).getOrElse("")} $label"
}

case class Compare(src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"CMP $src $op2"
}

case class Move(dest: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"MOV $dest $op2"
}
