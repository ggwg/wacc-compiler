import WACCParser._
import com.wacc._
import parsley._

object Playground {
  def testFunction(): Unit = {
    println("Testing Function:\n")
    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()
    topST.add("int", IntType(), null)
    // TODO: Define global data types as mentioned in slides.
    var parsedResult: Result[Function] =
      functionParser
        .runParser(
          "int fibonacci(int n, int j, int j) is " +
            "                    return 1 end"
        )
    var ast = parsedResult.get
    ast.check(topST)
    println("Finished Testing Function\n")
  }

  def main(args: Array[String]): Unit = {
    testFunction()
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
