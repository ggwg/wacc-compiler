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
    val filename = args(0)
    var input = ""
    for (line <- Source.fromFile(filename).getLines()) {
      input += line + "\n"
    }

    // Parsing
    val parseResult = WACCParser.programParser.runParser(input)
    if (parseResult.isFailure) {
      println("#syntax_error#\nexit:\n100")
      sys.exit(100)
    }

    // Semantic Analysis
    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()

    var res = parseResult.get
    implicit val semanticErrors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty
    res.check(topST)(semanticErrors)
    for (error <- semanticErrors) {
      error.throwError()
    }
    if (!semanticErrors.isEmpty) {
      sys.exit(200)
    }
  }
}
