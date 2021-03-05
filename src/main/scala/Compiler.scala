import com.wacc.{
  ADD,
  ArrayIndexBoundsError,
  ArrayIndexError,
  ArrayIndexNegativeError,
  AsciiDirective,
  AssemblerState,
  BRANCH,
  BRANCHLINK,
  COMPARE,
  CS,
  DataDirective,
  DivideByZeroError,
  EQ,
  Error,
  FreeNullArrayError,
  FreeNullPairError,
  ImmediateNumber,
  Instruction,
  LOAD,
  LT,
  MOVE,
  MessageLoad,
  NullDereferenceError,
  OverflowError,
  POP,
  PUSH,
  PopPC,
  PushLR,
  Register0,
  Register1,
  RegisterLoad,
  RegisterOffsetLoad,
  RegisterSP,
  RuntimeError,
  SUB,
  StringLabel,
  SymbolTable,
  WordDirective
}
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
      footer ++= List(
        StringLabel(DivideByZeroError.label),
        PushLR(),
        COMPARE(Register1, ImmediateNumber(0)),
        LOAD(Register0, MessageLoad(state.getMessageID(DivideByZeroError.errorMessage)), isByte = false, Option(EQ)),
        BRANCHLINK(RuntimeError.label, Option(EQ)),
        PopPC()
      )
    }
    if (state.p_throw_overflow_error) {
      footer ++= List(
        StringLabel(OverflowError.label),
        LOAD(Register0, MessageLoad(state.getMessageID(OverflowError.errorMessage))),
        BRANCHLINK(RuntimeError.label)
      )
    }
    if (state.p_free_pair) {
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
        BRANCHLINK(RuntimeError.label, Option(LT)),
        LOAD(Register1, RegisterLoad(Register1)),
        COMPARE(Register0, Register1),
        LOAD(
          Register0,
          MessageLoad(state.getMessageID(ArrayIndexBoundsError.errorMessage)),
          isByte = false,
          Option(CS)
        ),
        BRANCHLINK(RuntimeError.label, Option(CS)),
        PopPC()
      )
    }
    if (state.p_throw_runtime_error) {
      footer ++= List(
        StringLabel(RuntimeError.label),
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

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: ./compile <path_to_file>")
      return
    }

    val fileName = args(0)
    val split = fileName.split('.')

    /* Check that the input is correct */
    if (split.length != 2 || !split(1).equals("wacc")) {
      println("File path must be of the form 'path/name.wacc' or 'name.wacc'")
      return
    }

    /* Retrieve the base file name from the path */
    val baseName = split(0).split('/').last

    /* Parse the input */
    val parseResult = Parser.programParser.parseFromFile(new File(fileName))

    parseResult match {
      case Failure(msg) =>
        println("#syntax_error#\n" + msg)
        sys.exit(Error.syntaxCode)
      case Success(_) => ()
    }

    /* Semantic Analysis:
       Initialize top level Symbol Table */
    val topST: SymbolTable = new SymbolTable()

    val AST = parseResult.get
    implicit val semanticErrors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty

    /* Traverse the AST to identify any semantic errors and a few syntactic ones */
    AST.check(topST)(semanticErrors)

    /* Check for any syntax errors */
    val syntaxError = semanticErrors.find(error => error.code == Error.syntaxCode)
    syntaxError match {
      case Some(err) =>
        err.throwError()
        sys.exit(Error.syntaxCode)
      case None => ()
    }

    /* Check for semantic errors */
    semanticErrors.foreach(_.throwError())
    if (semanticErrors.nonEmpty) {
      sys.exit(Error.semanticCode)
    }

    /* Compile the program */
    val instructions: ListBuffer[Instruction] = ListBuffer.empty
    val state = AST.compile(AssemblerState.initialState)(instructions)

    /* Add the program headers. Program headers include messages */
    val header: ListBuffer[Instruction] = generateHeader(state)

    /* Add the program footers. Program footers include error labels */
    val footer: ListBuffer[Instruction] = generateFooter(state)

    val curatedInstructions: List[Instruction] = curateInstructions(instructions.toList)

    /* Write to an assembly file */
    writeToFile(baseName + ".s", header, curatedInstructions, footer)
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
