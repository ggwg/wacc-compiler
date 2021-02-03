import WACCParser._
import com.wacc._
import parsley._

object Playground {

  def main(args: Array[String]): Unit = {
    println("Start Main")

    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable(None)
    // TODO: Define global data types as mentioned in slides.

    var parsedResult: Result[Statement] =
      statementParser
        .runParser("int i = true")
    println(parsedResult)
    var ast = parsedResult.get
    ast.check(topST)
    println("End Main")
  }
}
