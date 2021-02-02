import WACCParser._
import com.wacc._
import parsley._

object Playground {

  def main(args: Array[String]): Unit = {
    println("Start Main")

    // Initialize top level Symbol Table
    var topST: SymbolTable = new SymbolTable(None)
    // TODO: Define global data types as mentioned in slides.

    var parsedResult: Result[Program] =
      programParser
        .runParser(
          "begin" +
            "     int pred(int x) is return (x-1) end" +
            "     int succ(int x) is return (x+1) end" +
            "     int a = 0;" +
            "     a = call succ(a);" +
            "     a = call pred(a);" +
            "     if (a == 0) then" +
            "       return 0" +
            "     else" +
            "       return -1" +
            "     fi" +
            "     end"
        )
    println(parsedResult)
    var astProgramType = parsedResult.get
    astProgramType.check(topST)

    println("End Main")
  }
}
