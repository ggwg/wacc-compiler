import com.wacc._
import parsley.{Failure, Success}

import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Compiler {
  /* Compute the effective message length, taking escaped chars into account*/
  def getMessageLength(message: String): Int = {
    var length = 0
    var isEscaped = false
    for (i <- message.indices) {
      if (isEscaped) {
        isEscaped = false
        length += 1
      } else if (message(i) == '\\') {
        isEscaped = true
      } else {
        length += 1
      }
    }
    length
  }

  def generateHeader(state: AssemblerState): ListBuffer[Instruction] = {
    var header: ListBuffer[Instruction] = ListBuffer.empty
    val labelPrefix = "msg_"

    /* Data directive */
    header += DataDirective()

    /* Add all the messages */
    for ((message, id) <- state.messageDic) {
      header += StringLabel(labelPrefix + id)

      /* Message length */
      val length = getMessageLength(message)

      /* Add the message's length and content, \0 included*/
      header ++= List(WordDirective(length + 1), AsciiDirective(message + "\\0"))
    }
    header
  }

  def generateFooter(state: AssemblerState): ListBuffer[Instruction] = {
    var footer: ListBuffer[Instruction] = ListBuffer.empty
    if (state.p_check_null_pointer) {
      /* r0 = pointer, r1 = catch label, r2 = sp offset for the label */
      footer ++= List(
        StringLabel(NullDereferenceError.label),
        PushLR(),
        COMPARE(Register0, ImmediateNumber(0)),
        LOAD(Register0, MessageLoad(state.getMessageID(NullDereferenceError.errorMessage)), isByte = false, Option(EQ)),
        BRANCHLINK(RuntimeError.label, Option(EQ)),
        PopPC()
      )
    }
    if (state.p_check_divide_by_zero) {
      /* r0 = denominator, r1 = numerator, r2 = catch label, r3 = sp offset for the label */
      footer ++= List(
        StringLabel(DivideByZeroError.label),
        PushLR(),
        COMPARE(Register1, ImmediateNumber(0)),
        LOAD(Register0, MessageLoad(state.getMessageID(DivideByZeroError.errorMessage)), isByte = false, Option(EQ)),
        MOVE(Register1, Register2, cond = Option(EQ)),
        MOVE(Register2, Register3, cond = Option(EQ)),
        BRANCHLINK(RuntimeError.label, Option(EQ)),
        PopPC()
      )
    }
    if (state.p_throw_overflow_error) {
      /* r0 = catch label, r1 = sp offset for the label */
      footer ++= List(
        StringLabel(OverflowError.label),
        PushLR(),
        MOVE(Register2, Register1),
        MOVE(Register1, Register0),
        LOAD(Register0, MessageLoad(state.getMessageID(OverflowError.errorMessage))),
        BRANCHLINK(RuntimeError.label),
        PopPC()
      )
    }
    if (state.p_free_pair) {
      /* r0 = pointer, r1 = catch label, r2 = sp offset for the label */
      footer ++= List(
        StringLabel(FreeNullPairError.label),
        PushLR(),
        COMPARE(Register0, ImmediateNumber(0)),
        LOAD(Register0, MessageLoad(state.getMessageID(FreeNullPairError.errorMessage)), isByte = false, Option(EQ)),
        BRANCH(Option(EQ), RuntimeError.label),
        /* Save the pair pointer on the stack */
        PUSH(Register0),
        /* Free the first pointer */
        LOAD(Register0, RegisterLoad(Register0)),
        BRANCHLINK("free"),
        /* Retrieve the pair pointer */
        LOAD(Register0, RegisterLoad(RegisterSP)),
        /* Free the second pointer */
        LOAD(Register0, RegisterOffsetLoad(Register0, ImmediateNumber(4))),
        BRANCHLINK("free"),
        /* Free the pair pointer */
        POP(Register0),
        BRANCHLINK("free"),
        PopPC()
      )
    }
    if (state.p_free_array) {
      /* r0 = pointer, r1 = catch label, r2 = sp offset for the label */
      footer ++= List(
        StringLabel(FreeNullArrayError.label),
        PushLR(),
        COMPARE(Register0, ImmediateNumber(0)),
        LOAD(Register0, MessageLoad(state.getMessageID(FreeNullArrayError.errorMessage)), isByte = false, Option(EQ)),
        BRANCH(Option(EQ), RuntimeError.label),
        BRANCHLINK("free"),
        PopPC()
      )
    }
    if (state.p_check_array_bounds) {
      /* r0 = index, r1 = array pointer, r2 = catch label, r3 = sp offset for the label */
      footer ++= List(
        StringLabel(ArrayIndexError.label),
        PushLR(),
        COMPARE(Register0, ImmediateNumber(0)),
        LOAD(
          Register0,
          MessageLoad(state.getMessageID(ArrayIndexNegativeError.errorMessage)),
          isByte = false,
          Option(LT)
        ),
        PUSH(Register1),
        PUSH(Register2),
        MOVE(Register1, Register2),
        MOVE(Register2, Register3),
        BRANCHLINK(RuntimeError.label, Option(LT)),
        POP(Register2),
        POP(Register1),
        LOAD(Register1, RegisterLoad(Register1)),
        COMPARE(Register0, Register1),
        LOAD(
          Register0,
          MessageLoad(state.getMessageID(ArrayIndexBoundsError.errorMessage)),
          isByte = false,
          Option(CS)
        ),
        MOVE(Register1, Register2),
        MOVE(Register2, Register3),
        BRANCHLINK(RuntimeError.label, Option(CS)),
        PopPC()
      )
    }
    if (state.p_throw_runtime_error) {
      /* r0 = message, r1 = catch label, r2 = sp offset for the label */
      footer ++= List(
        StringLabel(RuntimeError.label),
        ADD(Register0, Register0, ImmediateNumber(4)),
        COMPARE(Register1, ImmediateNumber(0)),
        ADD(RegisterSP, RegisterSP, Register2),
        ADD(RegisterSP, RegisterSP, ImmediateNumber(4)),
        BRANCHX(Register1, Some(NE)),
        SUB(RegisterSP, RegisterSP, ImmediateNumber(4)),
        SUB(RegisterSP, RegisterSP, Register2),
        BRANCHLINK("printf"),
        MOVE(Register0, ImmediateNumber(Error.runtimeCode)),
        BRANCHLINK("exit")
      )
    }
    footer
  }

  def curateInstructions(instructions: List[Instruction]): List[Instruction] = {
    if (instructions.isEmpty) {
      return List.empty
    }

    instructions.flatMap {
      case ADD(dst, src, ImmediateNumber(0)) =>
        if (src == dst) List.empty else List(MOVE(dst, src))
      case ADD(dst, src, ImmediateNumber(n)) =>
        var remaining = n
        var result: ListBuffer[Instruction] = ListBuffer.empty
        while (remaining >= 1024) {
          result += ADD(dst, src, ImmediateNumber(1024))
          remaining -= 1024
        }
        while (remaining < -1024) {
          result += SUB(dst, src, ImmediateNumber(1024))
          remaining += 1024
        }
        result += ADD(dst, src, ImmediateNumber(remaining))
        result.toList
      case SUB(dst, src, ImmediateNumber(0)) =>
        if (src == dst) List.empty else List(MOVE(dst, src))
      case SUB(dst, src, ImmediateNumber(n)) =>
        var remaining = n
        var result: ListBuffer[Instruction] = ListBuffer.empty
        while (remaining >= 1024) {
          result += SUB(dst, src, ImmediateNumber(1024))
          remaining -= 1024
        }
        while (remaining < -1024) {
          result += ADD(dst, src, ImmediateNumber(1024))
          remaining += 1024
        }
        result += SUB(dst, src, ImmediateNumber(remaining))
        result.toList
      case i => List(i)
    }
  }

  def generateAST(fileName: String, symbolTable: SymbolTable): Option[Program] = {
    val split = fileName.split('.')
    /* Check that the input is correct */
    if (split.length != 2 || !split(1).equals("wacc")) {
      println("File path must be of the form 'path/name.wacc' or 'name.wacc'")
      Option.empty
    }
    /* Parse the input */
    val parseResult = Parser.programParser.parseFromFile(new File(fileName))

    parseResult match {
      case Failure(msg) =>
        println("#syntax_error#\n" + msg)
        sys.exit(Error.syntaxCode)
      case Success(_) => ()
    }

    var AST = parseResult.get
    implicit val semanticErrors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty

    /* Traverse the AST to identify any semantic errors and a few syntactic ones */
    AST.check(symbolTable)(semanticErrors)

    /* Check for any syntax errors */
    val syntaxError = semanticErrors.find(error => error.code == Error.syntaxCode)
    syntaxError match {
      case Some(err) =>
        err.throwError()
        sys.exit(Error.syntaxCode)
      case None => ()
    }

    /* Remove unreachable statements */
    AST = AST.removeUnreachableStatements()

    /* Check for semantic errors */
    semanticErrors.foreach(_.throwError())
    if (semanticErrors.nonEmpty) {
      sys.exit(Error.semanticCode)
    }
    Option(AST)
  }

  /* getBaseName is only called after generateAST, which checks fileName is in the right format */
  def getBaseName(fileName: String): String = {
    val split = fileName.split('.')
    split(0).split('/').last
  }

  def main(args: Array[String]): Unit = {
    var state = AssemblerState.initialState
    /* Semantic Analysis: Initialize top level Symbol Table */
    val topST: SymbolTable = new SymbolTable()

    if (args.length < 1) {
      println("Usage: ./compile <path_to_file>")
      return
    }
    /* Compile additional libraries */
    if (args.length > 1) {
      for (i <- 1 until args.length) {
        val parsedImportAST: Option[Program] = generateAST(args(i), topST)
        val AST = parsedImportAST match {
          case Some(value) => value
          case None => return
        }

        val instructions = ListBuffer.empty[Instruction]
        for (function <- AST.functions) {
          state = state.putFunction(function.name.identifier, function.thisFunctionType)
          state = function.compile(state)(instructions)
        }

        /* Finally, add parsed program into assembler state */
        state = state.putImport(getBaseName(args(i)), instructions.toList)
      }
    }

    val parseMainAST: Option[Program] =generateAST(args(0), topST)
    val AST = parseMainAST match {
      case Some(value) => value
      case None => return
    }

    /* Compile the program */
    val instructions: ListBuffer[Instruction] = ListBuffer.empty
    state = AST.compile(state)(instructions)

    /* Add the program headers. Program headers include messages */
    val header: ListBuffer[Instruction] = generateHeader(state)

    /* Add the program footers. Program footers include error labels */
    val footer: ListBuffer[Instruction] = generateFooter(state)

    val curatedInstructions: List[Instruction] = curateInstructions(instructions.toList)

    /* Write to an assembly file */
    writeToFile(getBaseName(args(0)) + ".s", header, curatedInstructions, footer)
  }

  def writeToFile(
    assembledFileName: String,
    header: ListBuffer[Instruction],
    body: List[Instruction],
    footer: ListBuffer[Instruction]
  ): Unit = {
    val file = new File(assembledFileName)
    val writer = new FileWriter(file)

    /* Write all messages */
    header.foreach(instr => writer.write(instr.toString + "\n"))

    /* Add directives */
    writer.write(".text\n")
    writer.write(".global main\n")

    /* Write the program instructions */
    body.foreach(instr => writer.write(instr.toString + "\n"))
    footer.foreach(instr => writer.write(instr.toString + "\n"))
    writer.close()
  }
}
