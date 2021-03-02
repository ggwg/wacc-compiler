import com.wacc.{
  AsciiDirective,
  AssemblerState,
  BRANCH,
  BRANCHLINK,
  COMPARE,
  CS,
  DataDirective,
  EQ,
  Error,
  ImmediateNumber,
  Instruction,
  LOAD,
  LT,
  MOVE,
  MessageLoad,
  POP,
  PUSH,
  PopPC,
  PushLR,
  Register0,
  Register1,
  RegisterLoad,
  RegisterOffsetLoad,
  RegisterSP,
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

    /* Data directive */
    header += DataDirective()

    /* Add all the messages */
    for ((message, id) <- state.messageDic) {
      header += StringLabel("msg_" + id)

      /* Message length */
      val length = getMessageLength(message)

      /* Add the message's length and content, \0 included*/
      header += WordDirective(length + 1)
      header += AsciiDirective(message + "\\0")
    }
    header
  }

  def generateFooter(state: AssemblerState): ListBuffer[Instruction] = {
    var footer: ListBuffer[Instruction] = ListBuffer.empty
    // val overflowMessage = "OverflowError: the result is too small/large to store in a 4-byte signed-integer."
    if (state.p_check_null_pointer) {
      footer += StringLabel("p_check_null_pointer")
      footer += PushLR()
      footer += COMPARE(Register0, ImmediateNumber(0))
      footer += LOAD(
        Register0,
        MessageLoad(state.getMessageID(state.getNullReferenceMessage())),
        isByte = false,
        Option(EQ)
      )
      footer += BRANCHLINK("p_throw_runtime_error", Option(EQ))
      footer += PopPC()
    }
    if (state.p_check_divide_by_zero) {
      footer += StringLabel("p_check_divide_by_zero")
      footer += PushLR()
      footer += COMPARE(Register1, ImmediateNumber(0))
      footer += LOAD(
        Register0,
        MessageLoad(state.getMessageID(state.getDivideByZeroMessage())),
        isByte = false,
        Option(EQ)
      )
      footer += BRANCHLINK("p_throw_runtime_error", Option(EQ))
      footer += PopPC()
    }
    if (state.p_throw_overflow_error) {
      footer += StringLabel("p_throw_overflow_error")
      footer += LOAD(Register0, MessageLoad(state.getMessageID(state.getOverflowMessage())))
      footer += BRANCHLINK("p_throw_runtime_error")
    }
    if (state.p_free_pair) {
      footer += StringLabel("p_free_pair")
      footer += PushLR()
      footer += COMPARE(Register0, ImmediateNumber(0))
      footer += LOAD(Register0, MessageLoad(state.getMessageID(state.getNullReferenceMessage())))
      footer += BRANCHLINK("p_throw_runtime_error", Option(EQ))
      /* Save the pair pointer on the stack */
      footer += PUSH(Register0)

      /* Free the first pointer */
      footer += LOAD(Register0, RegisterLoad(Register0))
      footer += BRANCHLINK("free")

      /* Retrieve the pair pointer */
      footer += LOAD(Register0, RegisterLoad(RegisterSP))

      /* Free the second pointer */
      footer += LOAD(Register0, RegisterOffsetLoad(Register0, ImmediateNumber(4)))
      footer += BRANCHLINK("free")

      /* Free the pair pointer */
      footer += POP(Register0)
      footer += BRANCHLINK("free")
      footer += PopPC()
    }
    if (state.p_check_array_bounds) {
      footer += StringLabel("p_check_array_bounds")
      footer += PushLR()
      footer += COMPARE(Register0, ImmediateNumber(0))
      footer += LOAD(
        Register0,
        MessageLoad(state.getMessageID(state.getArrayNegativeIndexMessage())),
        isByte = false,
        Option(LT)
      )
      footer += BRANCHLINK("p_throw_runtime_error", Option(LT))
      footer += LOAD(Register1, RegisterLoad(Register1))
      footer += COMPARE(Register0, Register1)
      footer += LOAD(
        Register0,
        MessageLoad(state.getMessageID(state.getArrayIndexTooLargeMessage())),
        isByte = false,
        Option(CS)
      )
      footer += BRANCHLINK("p_throw_runtime_error", Option(CS))
      footer += PopPC()
    }
    if (state.p_throw_runtime_error) {
      footer += StringLabel("p_throw_runtime_error")
      footer += BRANCHLINK("printf")
      footer += MOVE(Register0, ImmediateNumber(-1))
      footer += BRANCHLINK("exit")
    }
    footer
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
    val parseResult = WACCParser.programParser.parseFromFile(new File(fileName))

    parseResult match {
      case Failure(msg) =>
        println("#syntax_error#\n" + msg)
        sys.exit(100)
      case Success(_) => ()
    }

    /* Semantic Analysis
    Initialize top level Symbol Table */
    val topST: SymbolTable = new SymbolTable()

    val AST = parseResult.get
    implicit val semanticErrors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty

    /* Traverse the AST to identify any semantic errors and a few
       syntactic ones */
    AST.check(topST)(semanticErrors)

    /* Check for any syntax errors */
    val syntaxError = semanticErrors.find(error => error.code == 100)
    syntaxError match {
      case Some(err) =>
        err.throwError()
        sys.exit(100)
      case None => ()
    }

    /* Check for semantic errors */
    semanticErrors.foreach(_.throwError())
    if (semanticErrors.nonEmpty) {
      sys.exit(200)
    }

    /* Compile the program */
    val instructions: ListBuffer[Instruction] = ListBuffer.empty
    var state = AST.compile(AssemblerState.initialState)(instructions)

    /* TODO: Add footers for the program */
    // TODO code goes here

    /* Add the program headers. Program headers include messages */
    val header: ListBuffer[Instruction] = generateHeader(state)

    /* Add the program footers. Program footers include error labels */
    val footer: ListBuffer[Instruction] = generateFooter(state)

    /* TODO: Split add and sub operations which use more than #1024 */

    /* Write to an assembly file */
    // TODO: Footer
    writeToFile(baseName + ".s", header, instructions, footer)
  }

  def writeToFile(
    assembledFileName: String,
    header: ListBuffer[Instruction],
    body: ListBuffer[Instruction],
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
