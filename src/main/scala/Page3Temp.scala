import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.functions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.statements._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.{Parsley, combinator}
import Parsley._
import parsley.character.{alphaNum, letter, noneOf}
import parsley.combinator.{manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._

import scala.io.Source

object Main2 {
  def main(args: Array[String]): Unit = {
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }
  }

  def square(x: Int): Int = x * x
}

/*
a <~> b -> pair(a, b)
a, b    -> a concat b
a *> b  -> b
a #> b  -> pattern match on a; if successful, replace with b
a <\> b -> a OR b
a <|> b -> try a until you fail; continue with b
 */
