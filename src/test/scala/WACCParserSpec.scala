import org.scalatest.flatspec.AnyFlatSpec
import WACCParser._
import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.functions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.statements._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.{Failure, Success}

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

  "An integer sign parser" should "only parse a '+' or '-'" in {
    assert(integerSignParser.runParser("+").isSuccess)
    assert(integerSignParser.runParser("-").isSuccess)
    assert(integerSignParser.runParser("x").isFailure)
    assert(integerSignParser.runParser("5").isFailure)
  }

  "A boolean literal parser" should "only parse a 'true' or 'false' string" in {
    assert(booleanLiterParser.runParser("true").isSuccess)
    assert(booleanLiterParser.runParser("false").isSuccess)
    assert(booleanLiterParser.runParser("xyz").isFailure)
    assert(booleanLiterParser.runParser("tru").isFailure)
  }

  "A character literal parser" should "read a single default character surrounded in '\''" in {
    val parsed = characterLiterParser.runParser("\'x\'")
    parsed match {
      case Success(character) =>
        assert(character.char == 'x')
      case Failure(msg) => fail(msg)
    }
  }
  it should "fail to parse anything missing the simple quotes" in {
    assert(characterLiterParser.runParser("\'x").isFailure)
    assert(characterLiterParser.runParser("x\'").isFailure)
    assert(characterLiterParser.runParser("x").isFailure)
    assert(characterLiterParser.runParser("xyz").isFailure)
  }

  "A string literal parser" should "parse any string enclosed in '\"'" in {
    val parsed = stringLiterParser.runParser("\"This is a valid string\"")
    parsed match {
      case Success(string) =>
        assert(string.string.equals("This is a valid string"))
      case Failure(msg) => fail(msg)
    }
  }
  it should "fail to parse any string missing a '\"'" in {
    assert(stringLiterParser.runParser("\"No end quote").isFailure)
    assert(stringLiterParser.runParser("No beggining quote\"").isFailure)
  }

  "A default character parser" should "parse any character except '\\', '\'' and '\"'" in {
    assert(defaultCharacterParser.runParser("a").isSuccess)
    assert(defaultCharacterParser.runParser("Z").isSuccess)
    assert(defaultCharacterParser.runParser("9").isSuccess)
    assert(defaultCharacterParser.runParser("@").isSuccess)
    assert(defaultCharacterParser.runParser("[").isSuccess)
  }
  it should "fail to parse '\\', '\'' and '\"'" in {
    assert(defaultCharacterParser.runParser("\\").isFailure)
    assert(defaultCharacterParser.runParser("\'").isFailure)
    assert(defaultCharacterParser.runParser("\"").isFailure)
  }
  it should "only parse escapable characters after '\\'" in {
    for (chr <- EscapedCharacter.escapableCharacters) {
      assert(defaultCharacterParser.runParser("\\" + chr).isSuccess)
    }
    assert(defaultCharacterParser.runParser("\\a").isFailure)
  }

  "An escaped character parser" should "only parse escapable characters" in {
    for (chr <- EscapedCharacter.escapableCharacters) {
      val parsed = escapedCharParser.runParser(chr.toString)
      assert(parsed.isSuccess)
    }

    assert(escapedCharParser.runParser("5").isFailure)
    assert(escapedCharParser.runParser("a").isFailure)
    assert(escapedCharParser.runParser("X").isFailure)
  }

  "An array literal parser" should "" in {}

  "A pair literal parser" should "only parse a 'null' string" in {
    val parsed1 = pairLiterParser.runParser("null")
    parsed1 match {
      case Success(_)   =>
      case Failure(msg) => fail(msg)
    }

    val parsed2 = pairLiterParser.runParser("not a pair liter")
    val parsed3 = pairLiterParser.runParser("nul")

    assert(parsed2.isFailure && parsed3.isFailure)
  }

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
