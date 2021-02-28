import com.wacc
import com.wacc._
import com.wacc.operator.{BinaryOperator, UnaryOperator}
import parsley.Parsley._
import parsley.character.{alphaNum, anyChar, letter, noneOf, satisfy, whitespace}
import parsley.combinator.{attemptChoice, eof, manyN, option}
import parsley.expr.{InfixL, Ops, Postfix, Prefix, precedence}
import parsley.implicits.{voidImplicitly => _, _}
import parsley.{Failure, Parsley, combinator}
import java.io.{File, FileWriter}

import WACCParser.{programParser, skipWhitespace}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object BackEndTest {

  /*
  Automatically generates all assembly files for a given directory into the
  corresponding directory in the src/main/out folder.

  Make sure that the inputDir matches the right folder for outputDir.
  E.g.: val inputDir  = "src/main/resources/valid_examples/basic"
        val outputDir = "src/main/out/valid_examples/basic"
   */

  def main(args: Array[String]): Unit = {
    val inputDir  = "src/main/resources/valid_examples/if"
    val outputDir = "src/main/out/valid_examples/if"

    val resources = new File(inputDir)
    compileAll(resources, outputDir)
  }

  def compileAll(dir: File, outputDir: String): Unit = {
    for (file <- dir.listFiles()) {
      if (file.isDirectory) {
        val newDirectoryName = file.toString splitAt (file.toString lastIndexOf '\\')

        compileAll(file, outputDir + newDirectoryName._2)
      } else {
        val fileName = file.getAbsolutePath
        compileIndividual(fileName, outputDir)
      }
    }
  }

  def compileIndividual(dir: String, outputDir: String): Unit = {
    val fileName = dir
    val split = fileName.split('.')

    /* Check that the input is correct */
    if (split.length != 2 || !split(1).equals("wacc")) {
      println("File path must be of the form 'path/name.wacc' or 'name.wacc'")
      return
    }

    /* Retrieve the base file name from the path */
    val baseName = split(0).split('/').last

    val fileNameWithoutDirectory = (baseName splitAt (baseName lastIndexOf '\\'))._2
    val output = outputDir + fileNameWithoutDirectory + ".s"


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
    val assembledFileName = output
    val file = new File(assembledFileName)
    val writer = new FileWriter(file)
    header.foreach(i => writer.write(i.toString + "\n"))
    writer.write(".global main\n")
    instructions.foreach(i => writer.write(i.toString + "\n"))
    writer.close()
  }
}

