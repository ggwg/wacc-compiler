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
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }

    val test: Parsley[String] =
      ("a" <~> "b" <~> ("c" *> "d")).map { case ((x, y), z) => x + y + z }
    println(test.runParser("abc"))

  }

  def square(x: Int): Int = x * x
}
