import com.wacc.{Error, SymbolTable}
import parsley.Failure

import scala.collection.mutable
import scala.io.Source

object Compiler {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: ./compiler <path_to_file>")
      return
    }

    /* Read the file into the input string */
    val filename = args(0)
    var input = ""
    for (line <- Source.fromFile(filename).getLines()) {
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

    /* TODO: Output AST to assembly */
  }
}
