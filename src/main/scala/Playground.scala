import WACCParser._
import com.wacc._
import parsley._

import scala.collection.mutable

object Playground {
  def testProgram(): Unit = {
    println("Testing Program...\n")
    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()

    var parsedResult: Result[Program] = {
      programParser
        .runParser(
          "begin" +
            "     if (a == 0) then" +
            "       return 0" +
            "     else" +
            "       return -1" +
            "     fi" +
            "     end"
//          "begin" +
//            "     int pred(int x, int j, char x) is return (x-1) end" +
//            "     int succ(int x) is return (x+1) end" +
//            "     int a = 0;" +
//            "     int a = 2;" +
//            "     a = call succ(a);" +
//            "     a = call pred(a);" +
//            "     if (a == 0) then" +
//            "       return 0" +
//            "     else" +
//            "       return -1" +
//            "     fi" +
//            "     end"
        )
    }
    var ast = parsedResult.get
    implicit val errors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty
    ast.check(topST)(errors)
    for (error <- errors) {
      error.throwError()
    }
    println("Finished Testing Program\n")
  }

  def testFunction(): Unit = {
    println("Testing Function:\n")
    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable()
    var parsedResult: Result[Function] =
      functionParser
        .runParser(
          "int fibonacci(int n, int j, char j) is " +
            "                    return 1 end" +
            "                                "
        )
    var ast = parsedResult.get
    // ast.check(topST)
    println("Finished Testing Function\n")
  }

  def main(args: Array[String]): Unit = {
    testProgram()
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
