import WACCParser._
import com.wacc._
import parsley._

object Playground {

  def main(args: Array[String]): Unit = {
    println("Start Main")

    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()
    // TODO: Define global data types as mentioned in slides.

    var parsedResult: Result[Statement] =
      statementParser
        .runParser("int x = 10 ; int x = 10")
        // we should run
    println(parsedResult)
    // Exit if there was a syntax error with code 100

    var ast = parsedResult.get
    ast.check(topST)
    println("End Main")

    /*
    enum that indicates the different types:
    int, bool, char, string, array, T

    typeTable = ["int", "bool", "char", "string", "array", ...
                 (user-defined stuff would be appended here)]

    symbolTable = [("x", 0), ]
    symbolTable = [("var_name":(UserType, ASTNode))]

    */


//    // Type matching
//    var x: CharacterLiter = CharacterLiter('a')
//    var y: Any = x
//    println(x.getClass)
//    println(y.getClass)
//    println(x.getClass == y.getClass)
//    println(x.getClass.equals(y.getClass))
//    println("GOT TO END")
  }
}
