import parsley.Parsley
import Parsley._
import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.character.{alphaNum, letter, noneOf}
import parsley.combinator
import parsley.combinator.{manyN, option}
import parsley.expr
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file1 = "src/main/resources/valid_examples/basic/skip/skip.wacc"
    for (line <- Source.fromFile(file1).getLines()) {
      println(line)
    }

    val file2 = "src/main/resources/valid_examples/function/simple_functions/functionSimple.wacc"
    for (line <- Source.fromFile(file2).getLines()) {
      println(line)
    }

    val file3 = "src/main/resources/invalid_examples/syntaxErr/basic/badComment.wacc"
    for (line <- Source.fromFile(file3).getLines()) {
      println(line)
    }

    val test: Parsley[String] =
      ("a" <~> "b" <~> ("c" *> "d")).map { case ((x, y), z) => x + y + z }
    println(test.runParser("abc"))

  }

  def square(x: Int): Int = x * x
}
