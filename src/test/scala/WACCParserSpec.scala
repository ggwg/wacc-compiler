import org.scalatest.flatspec.AnyFlatSpec
import WACCParser._
import com.wacc.miscellaneous.Comment
import parsley.{Success, Failure}

class WACCParserSpec extends AnyFlatSpec {
  val commentInput = "# This is a comment \n"
  "A program parser" should "" in {}

  "A function parser" should "" in {}

  "A parameter list parser" should "" in {}

  "A parameter parser" should "" in {}

  "A statement parser" should "" in {}

  "An assign left parser" should "" in {}

  "An assign right parser" should "" in {}

  "An argument list parser" should "" in {}

  "A pair element parser" should "" in {}

  "A type parser" should "" in {}

  "A base type parser" should "" in {}

  "An array type parser" should "" in {}

  "A pair type parser" should "" in {}

  "A pair element type parser" should "" in {}

  "An expression parser" should "" in {}

  "An unary operator parser" should "" in {}

  "A binary operator parser" should "" in {}

  "An identifier parser" should "" in {}

  "An array element parser" should "" in {}

  "An integer literal parser" should "" in {}

  "A digit parser" should "" in {}

  "An integer sign parser" should "" in {}

  "A boolean literal parser" should "" in {}

  "A character literal parser" should "" in {}

  "A string literal parser" should "" in {}

  "A default character parser" should "" in {}

  "An escaped character parser" should "" in {}

  "An array literal parser" should "" in {}

  "A pair literal parser" should "" in {}

  "A comment parser" should "parse a '#'" in {
    val parsed = commentParser.runParser("#")
    parsed match {
      case Success(comment) => assert(comment.comment.isEmpty)
      case Failure(msg)     => fail(msg)
    }
  }
  it should "parse any number of characters after a '#'" in {
    val parsed = commentParser.runParser("#comment!")
    parsed match {
      case Success(comment) => assert(comment.comment.equals("comment!"))
      case Failure(msg)     => fail(msg)
    }
  }
  it should "stop after reaching an EOL character" in {
    val parsed = commentParser.runParser(commentInput)
    parsed match {
      case Success(comment) =>
        assert(comment.comment.equals(" This is a comment "))
      case Failure(msg) => fail(msg)
    }
  }
  it should "fail to parse a string not starting with '#'" in {
    val parsed = commentParser.runParser("bad comment")
    parsed match {
      case Success(_) => fail("Parsed a non-comment")
      case Failure(_) =>
    }
  }
}
