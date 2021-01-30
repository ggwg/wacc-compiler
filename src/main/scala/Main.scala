import com.wacc.assignment.ArrayLiter
import com.wacc.expressions.{Expression, StringLiter}
import com.wacc.functions.Program
import com.wacc.miscellaneous.Comment
import com.wacc.primitives.EscapedCharacter
import parsley.Parsley
import parsley.expr.precedence
import parsley.implicits._

import scala.io.Source

object Main {

    def main(args: Array[String]): Unit = {
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }

    val toComment = (comment: String) => new Comment("")

    lazy val expressionParser = precedence[Expression](
    )

    lazy val escapedCharParser: Parsley[EscapedCharacter] =
      toEscapedCharacter.lift("")
      toEscapedCharacter.lift("")
      toEscapedCharacter.lift("")
      toEscapedCharacter.lift("")
      toEscapedCharacter.lift("")
    lazy val arrayLiterParser: Parsley[ArrayLiter] = toArrayLiter.lift("")
    lazy val stringLiterParser: Parsley[StringLiter] = toStringLiter.lift("")
    lazy val commentParser: Parsley[Comment] = toComment.lift("")

    commentParser.runParser("")
  }

  def square(x: Int): Int = x * x
}
