import org.scalatest.flatspec.AnyFlatSpec
import WACCParser._
import com.wacc.operator._
import com.wacc.EscapedCharacter
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

  "A type parser" should "parse all valid types" in {
    assert(typeParser.runParser("int").isSuccess)
    assert(typeParser.runParser("bool[][]").isSuccess)
    assert(typeParser.runParser("pair(int, char)").isSuccess)
    assert(typeParser.runParser("pair(pair, string[][])[]").isSuccess)
  }

  "A base type parser" should "parse all base types" in {
    assert(baseTypeParser.runParser("int").isSuccess)
    assert(baseTypeParser.runParser("bool").isSuccess)
    assert(baseTypeParser.runParser("char").isSuccess)
    assert(baseTypeParser.runParser("string").isSuccess)
  }

  "An array type parser" should "parse any array type" in {
    assert(arrayTypeParser.runParser("int[]").isSuccess)
    assert(arrayTypeParser.runParser("string[][]").isSuccess)
    assert(arrayTypeParser.runParser("pair(int, int)[]").isSuccess)
    assert(arrayTypeParser.runParser("pair(pair, int[])[]").isSuccess)
  }

  "A pair type parser" should "parse any valid pair type" in {
    assert(pairTypeParser.runParser("pair(int, char)").isSuccess)
    assert(pairTypeParser.runParser("pair(string, string)").isSuccess)
    assert(pairTypeParser.runParser("pair(pair, pair)").isSuccess)
    assert(pairTypeParser.runParser("pair(int[], string[][])").isSuccess)
  }

  "A pair element type parser" should "parse any valid pair element type" in {
    assert(pairElementTypeParser.runParser("int").isSuccess)
    assert(pairElementTypeParser.runParser("int[]").isSuccess)
    assert(pairElementTypeParser.runParser("pair").isSuccess)
  }

  "An expression parser" should "parse any literal" in {
    assert(expressionParser.runParser("-100").isSuccess)
    assert(expressionParser.runParser("true").isSuccess)
    assert(expressionParser.runParser("'x'").isSuccess)
    assert(expressionParser.runParser("\"string\"").isSuccess)
    assert(expressionParser.runParser("null").isSuccess)
    assert(expressionParser.runParser("identifier").isSuccess)
    assert(expressionParser.runParser("array[10][10]").isSuccess)
  }
  it should "parse any unary operator application" in {
    assert(expressionParser.runParser("!true").isSuccess)
    assert(expressionParser.runParser("--100").isSuccess)
    assert(expressionParser.runParser("len(\"thisstring\")").isSuccess)
    assert(expressionParser.runParser("ord('x')").isSuccess)
    assert(expressionParser.runParser("chr(100)").isSuccess)
  }
  it should "parse any binary operator application" in {
    for (op <- BinaryOperator.operators) {
      assert(expressionParser.runParser("5 " + op + " 5").isSuccess)
    }
  }
  it should "parse any complex expression" in {
    assert(expressionParser.runParser("(1 + 2) * (3 + 4)").isSuccess)
    assert(expressionParser.runParser("len(\"this\") + chr(111)").isSuccess)
    assert(
      expressionParser
        .runParser("true && (false || (false && (ident)))")
        .isSuccess
    )
    assert(expressionParser.runParser("(((((100)))))").isSuccess)
  }

  "An unary operator parser" should "parse all unary operators" in {
    for (op <- UnaryOperator.operators) {
      println(op)
      assert(unaryOperatorParser.runParser(op).get.equals(UnaryOperator(op)))
    }
  }
  it should "not parse anything else" in {
    assert(unaryOperatorParser.runParser("xyz").isFailure)
    assert(unaryOperatorParser.runParser("123").isFailure)
    assert(unaryOperatorParser.runParser("\n ==").isFailure)
  }

  "A binary operator parser" should "parse all binary operators" in {
    for (op <- BinaryOperator.operators) {
      println(op)
      assert(binaryOperatorParser.runParser(op).get.equals(BinaryOperator(op)))
    }
  }
  it should "not parse anything else" in {
    assert(binaryOperatorParser.runParser("xyz").isFailure)
    assert(binaryOperatorParser.runParser("123").isFailure)
    assert(binaryOperatorParser.runParser("\n ==").isFailure)
  }

  "An identifier parser" should "parse valid one character strings" in {
    assert(identifierParser.runParser("_").isSuccess)
    assert(identifierParser.runParser("x").isSuccess)
    assert(identifierParser.runParser("X").isSuccess)
  }
  it should "parse any alphanumeric valid identifier string" in {
    assert(identifierParser.runParser("aVariableName").isSuccess)
    assert(
      identifierParser.runParser("aNameContainingNumbers0123456789").isSuccess
    )
    assert(identifierParser.runParser("_NameContainingUnderscore_").isSuccess)
    assert(identifierParser.runParser("RandomLetters_34fsdf5JF9e7").isSuccess)
  }
  it should "fail to parse strings starting with digits" in {
    assert(identifierParser.runParser("0name").isFailure)
    assert(identifierParser.runParser("4name").isFailure)
    assert(identifierParser.runParser("9name").isFailure)
  }
  it should "fail to parse any keyword" in {
    for (keyword <- keywords) {
      assert(identifierParser.runParser(keyword).isFailure)
    }
  }
  it should "parse identifiers with keywords as substrings" in {
    assert(identifierParser.runParser("truetrue").isSuccess)
    assert(identifierParser.runParser("while0").isSuccess)
    assert(identifierParser.runParser("xlen").isSuccess)
  }

  "An array element parser" should "allow any array access of the form arrayName[expr1][expr2]...[exprN]" in {
    assert(arrayElementParser.runParser("a[0]").isSuccess)
    assert(arrayElementParser.runParser("aName[100]").isSuccess)
    assert(arrayElementParser.runParser("arrayName[10+10]").isSuccess)
    assert(arrayElementParser.runParser("a[100][100][100][100]").isSuccess)
    assert(arrayElementParser.runParser("a[1+2+3][5-2][a]").isSuccess)
  }
  it should "fail to parse any badly constructed array access" in {
    assert(arrayElementParser.runParser("a[]").isFailure)
    assert(arrayElementParser.runParser("[]").isFailure)
    assert(arrayElementParser.runParser("a[").isFailure)
    assert(arrayElementParser.runParser("a[]]").isFailure)
    assert(arrayElementParser.runParser("a[\n]").isFailure)
  }

  "An integer literal parser" should "parse any number (sequence of digits)" in {
    assert(integerLiterParser.runParser("0").isSuccess)
    assert(integerLiterParser.runParser("100").isSuccess)
    assert(integerLiterParser.runParser("123456789").isSuccess)
  }
  it should "parse any number preceded by a '+' or '-'" in {
    assert(integerLiterParser.runParser("+100").isSuccess)
    assert(integerLiterParser.runParser("-100").isSuccess)
  }
  it should "fail to parse multiple signs" in {
    assert(integerLiterParser.runParser("++100").isFailure)
    assert(integerLiterParser.runParser("--100").isFailure)
  }

  "A digit parser" should "only parse digits" in {
    for (digit <- "0123456789") {
      assert(digitParser.runParser(digit + "").isSuccess)
    }
    assert(digitParser.runParser("x").isFailure)
  }

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
    val parsed = commentParser.runParser("#\n")
    parsed match {
      case Success(comment) => assert(comment.comment.isEmpty)
      case Failure(msg)     => fail(msg)
    }
  }
  it should "parse any number of characters after a '#'" in {
    val parsed = commentParser.runParser("#comment!\n")
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
