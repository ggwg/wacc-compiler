import WACCParser._
import com.wacc._

object Playground {

  def main(args: Array[String]): Unit = {
    println("Start Main")

    var ST: SymbolTable = new SymbolTable(null)
    ST.add("Hello", CharacterLiter('c'))
    println(ST)
    println(ST.lookup("Hello"))
    println(ST.lookup("hello"))

    println(programParser
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
      ))
      println(functionParser
        .runParser(
          "int fibonacci(int n) is " +
            "                  if (n < 2) then" +
            "                    return 1" +
            "                  else" +
            "                    int fib1 = call fibonacci(n-1);" +
            "                    int fib2 = call fibonacci(n-2); " +
            "                    return (fib1 + fib2)" +
            "                  fi end "
        ))




      println("End Main")
  }
}
