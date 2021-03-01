package com.wacc

import scala.collection.mutable.ListBuffer

sealed trait Instruction
sealed trait Operand2
sealed trait AddressAccess
sealed trait Condition
sealed trait Register extends Operand2
sealed trait ImmediateValue extends Operand2
sealed trait Label extends Instruction
sealed trait Directive extends Instruction

class Instructions() {
  var instructions: ListBuffer[Instruction] = new ListBuffer[Instruction]

  def addInstruction(instruction: Instruction) = instructions.addOne(instruction)

  override def toString: String = {
    val str = new StringBuilder()
    for (instruction <- instructions) {
      str.addAll(instruction.toString)
    }
    str.toString()
  }
}

case class NumberLabel(value: Int) extends Label {
  override def toString: String = "L" + value + ":"
}

case class StringLabel(value: String) extends Label {
  override def toString: String = value + ":"
}

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

case class ImmediateNumber(value: Int) extends ImmediateValue {
  override def toString: String = "#" + value
}

case class ImmediateChar(chr: Char) extends ImmediateValue {
  override def toString: String = s"#'$chr'"
}

case class MessageLoad(id: Int) extends AddressAccess {
  override def toString: String = s"=msg_$id"
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
  override def toString: String = "\tPUSH {LR}"
}

case class PopPC() extends Instruction {
  override def toString: String = "\tPOP {PC}"
}

case class PUSH(reg: Register) extends Instruction {
  override def toString: String = s"\tPUSH {$reg}"
}

case class POP(reg: Register) extends Instruction {
  override def toString: String = s"\tPOP {$reg}"
}

case class ADD(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tADD $dest, $src, $op2"
}

case class ADDS(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tADDS $dest, $src, $op2"
}

case class ADDLSL(dest: Register, src: Register, op2: Operand2, shift: ImmediateValue) extends Instruction {
  override def toString: String = s"\tADD $dest, $src, $op2, LSL $shift"
}

case class SUB(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tSUB $dest, $src, $op2"
}

case class SUBS(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tSUBS $dest, $src, $op2"
}

case class ReverseSUB(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tRSB $dest, $src, $op2"
}

case class AND(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tAND $dest, $src, $op2"
}

case class OR(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tORR $dest, $src, $op2"
}

case class XOR(dest: Register, src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tEOR $dest, $src, $op2"
}

case class MUL(dest: Register, multiplier: Register, src: Register) extends Instruction {
  override def toString: String = s"\tMUL $dest, $multiplier, $src"
}

case class LOAD(dest: Register, src: AddressAccess, isByte: Boolean = false, cond: Option[Condition] = Option.empty)
    extends Instruction {
  override def toString: String = s"\tLDR${cond.map(_.toString).getOrElse("")}${if (isByte) "SB" else ""} $dest, $src"
}

case class STORE(src: Register, dest: AddressAccess, isByte: Boolean = false, cond: Option[Condition] = Option.empty)
    extends Instruction {
  override def toString: String = s"\tSTR${cond.map(_.toString).getOrElse("")}${if (isByte) "B" else ""} $src, $dest"
}

case class BRANCH(cond: Option[Condition], label: String) extends Instruction {
  override def toString: String = s"\tB${cond.map(_.toString).getOrElse("")} $label"
}

case class BRANCHLINK(label: String) extends Instruction {
  override def toString: String = "\tBL " + label
}

case class BLVS(label: String) extends Instruction {
  override def toString: String = "\tBLVS " + label
}

case class COMPARE(src: Register, op2: Operand2) extends Instruction {
  override def toString: String = s"\tCMP $src, $op2"
}

case class MOVE(dest: Register, op2: Operand2, cond: Option[Condition] = Option.empty) extends Instruction {
  override def toString: String = s"\tMOV${cond.map(_.toString).getOrElse("")} $dest, $op2"
}

case class DataDirective() extends Directive {
  override def toString: String = ".data"
}

case class LtorgDirective() extends Directive {
  override def toString: String = "\t.ltorg"
}

case class WordDirective(size: Int) extends Directive {
  override def toString: String = s"\t.word $size"
}

case class AsciiDirective(message: String) extends Directive {
  override def toString: String = "\t.ascii \"" + message + "\""
}

object Register {
  val usableRegisters: List[Register] =
    List(Register4, Register5, Register6, Register7, Register8, Register10, Register11)
}
