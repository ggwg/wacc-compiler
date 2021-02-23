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
  override def toString: String = "r11"
}

case object RegisterSP extends Register {
  override def toString: String = "sp"
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

/* Equal condition */
case object EQ extends Condition {
  override def toString: String = "EQ"
}

/* Not equal condition */
case object NE extends Condition {
  override def toString: String = "NE"
}

/* Less or equal condition */
case object LE extends Condition {
  override def toString: String = "LE"
}

/* Greater or equal condition */
case object GE extends Condition {
  override def toString: String = "GE"
}

/* Less than condition */
case object LT extends Condition {
  override def toString: String = "LT"
}

/* Greater than condition */
case object GT extends Condition {
  override def toString: String = "GT"
}

case class PushLR() extends Instruction {
  override def toString: String = "PUSH {LR}"
}

case class PopPC() extends Instruction {
  override def toString: String = "POP {PC}"
}

case class ADD(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"ADD $dest, $src, $op2"
}

case class ADDLSL(dest: Register, src: Register, op2: Operand2, shift: ImmediateValue) extends Instruction {
  override def toString: String = s"ADD $dest, $src, $op2, LSL $shift"
}

case class SUB(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"SUB $dest, $src, $op2"
}

case class ReverseSUB(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"RSB $dest, $src, $op2"
}

case class AND(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"AND $dest, $src, $op2"
}

case class OR(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"ORR $dest, $src, $op2"
}

case class XOR(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"EOR $dest, $src, $op2"
}

case class MUL(dest: Register, multiplier: Register, src: Register) extends Instruction {
  override def toString: String = s"MUL $dest, $multiplier, $src"
}

case class LOAD(dest: Register, src: AddressAccess) extends Instruction {
  override def toString: String = s"LDR $dest, $src"
}

case class STORE(src: Register, dest: AddressAccess) extends Instruction {
  override def toString: String = s"STR $src, $dest"
}

case class BRANCH(cond: Option[Condition], label: String) extends Instruction {
  override def toString: String = s"B${cond.map(_.toString).getOrElse("")} $label"
}

case class COMPARE(src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"CMP $src, $op2"
}

case class MOVE(dest: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"MOV $dest, $op2"
}
