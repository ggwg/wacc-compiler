import WACCParser._
import com.wacc._
import parsley._

object Playground {

  def main(args: Array[String]): Unit = {
    println("Start Main\n")

    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()
    topST.add("int", VoidType(), null)
    // TODO: Define global data types as mentioned in slides.

    var parsedResult: Result[Statement] =
      statementParser
        .runParser("int x = 10 ; z = 5")
        // we should run
    println(parsedResult)

    var ast = parsedResult.get
    ast.check(topST)
    println("\nEnd Main")

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
