import com.wacc.{
  AsciiDirective,
  AssemblerState,
  DataDirective,
  Error,
  Instruction,
  MessageLoad,
  PopPC,
  StringLabel,
  SymbolTable,
  WordDirective
}
import parsley.Failure

import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Compiler {
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

    /* Read the file into the input string */
    var input = ""
    for (line <- Source.fromFile(fileName).getLines()) {
      input += line + "\n"
    }

    /* Parse the input */
    val parseResult = WACCParser.programParser.runParser(input)
    if (parseResult.isFailure) {
      println("#syntax_error#\n" + parseResult.asInstanceOf[Failure].msg)
      sys.exit(100)
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
    for (error <- semanticErrors) {
      error.throwError()
    }
    if (semanticErrors.nonEmpty) {
      sys.exit(200)
    }

    /* Compile the program */
    val instructions: ListBuffer[Instruction] = ListBuffer.empty
    var finalState = AST.compile(AssemblerState.initialState)(instructions)

    val header: ListBuffer[Instruction] = ListBuffer.empty

    /* Data directive */
    header += DataDirective()

    /* Add all the messages */
    for ((message, id) <- finalState.messageDic) {
      header += StringLabel("msg_" + id)

      /* Compute the effective message length, taking escaped chars into account*/
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
      header += WordDirective(length)
      header += AsciiDirective(message)
    }

    /* TODO: Add footers for the program */

    /* Write to an assembly file */
    val assembledFileName = baseName + ".s"
    val file = new File(assembledFileName)
    val writer = new FileWriter(file)
    header.foreach(i => writer.write(i.toString + "\n"))
    writer.write(".global main\n")
    instructions.foreach(i => writer.write(i.toString + "\n"))
    writer.close()
  }
}
