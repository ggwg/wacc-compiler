import parsley.Failure

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

    val parseResult = WACCParser.programParser.runParser(input)
    if (parseResult.isFailure) {
      println("#syntax_error#\nexit:\n100")
//      sys.error("Exit code 100 returned\n" + parseResult.asInstanceOf[Failure].msg)
      sys.exit(100)
    }

    // TODO: Semantic check
  }
}
