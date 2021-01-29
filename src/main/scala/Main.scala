import com.wacc.functions.Program

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }
  }

  def square(x: Int): Int = x * x
}
