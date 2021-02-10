import org.scalatest.flatspec.AnyFlatSpec
import WACCParser._
import com.wacc.operator._
import com.wacc._
import org.scalatest.matchers.should.Matchers.{a, an, convertToAnyShouldWrapper}
import parsley.{Failure, Success}

class WACCParserSpec extends AnyFlatSpec {
  "A program parser" should "parse any program" in {
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
      .get shouldBe a[Program]
  }

  "A function parser" should "parse any valid function declaration" in {
    functionParser
      .runParser(
        "int fibonacci(int n) is " +
          "                  if (n < 2) then" +
          "                    return 1" +
          "                  else" +
          "                    int fib1 = call fibonacci(n-1);" +
          "                    int fib2 = call fibonacci(n-2); " +
          "                    return (fib1 + fib2)" +
          "                  fi end "
      )
      .get shouldBe a[Function]
  }

  "A parameter list parser" should "parse any list of parameters" in {
    parameterListParser.runParser("char c").get shouldBe a[ParameterList]
    parameterListParser
      .runParser("int myInt, char myChar, bool myBool")
      .get shouldBe a[ParameterList]
  }

  "A parameter parser" should "parse any parameter declaration" in {
    parameterParser.runParser("int x").get shouldBe a[Parameter]
    parameterParser.runParser("char[] array").get shouldBe a[Parameter]
    parameterParser
      .runParser("pair(bool[], pair) myPair")
      .get shouldBe a[Parameter]
  }

  "A statement parser" should "parse the skip statement" in {
    statementParser.runParser("skip").get shouldBe a[SkipStatement]
  }
  it should "parse the identifier declaration statement" in {
    statementParser
      .runParser("int x = 10")
      .get shouldBe a[IdentifierDeclaration]
    statementParser
      .runParser(
        "pair(int, int) myPair = newpair(10, 10)"
      )
      .get shouldBe a[IdentifierDeclaration]
  }
  it should "parse the assignment statement" in {
    statementParser.runParser("ident = 10").get shouldBe a[Assignment]
    statementParser
      .runParser("array[10][10] = [1, 2, 3]")
      .get shouldBe a[Assignment]
  }
  it should "parse a read statement" in {
    statementParser.runParser("read ident").get shouldBe a[Read]
  }
  it should "parse a free statement" in {
    statementParser.runParser("free ident").get shouldBe a[Free]
  }
  it should "parse a return statement" in {
    statementParser.runParser("return 0").get shouldBe a[Return]
  }
  it should "parse an exit statement" in {
    statementParser.runParser("exit 0").get shouldBe an[Exit]
  }
  it should "parse a print statement" in {
    statementParser
      .runParser("print \"a printed string\"")
      .get shouldBe a[Print]
  }
  it should "parse a println statement" in {
    statementParser
      .runParser("println \"a printed string\"")
      .get shouldBe a[Println]
  }
  it should "parse an if statement" in {
    statementParser
      .runParser("if (a > b) then println a else println b fi")
      .get shouldBe an[If]
  }
  it should "parse a while statement" in {
    statementParser
      .runParser("while (i < len(\"string\")) do i = i + 1 done")
      .get shouldBe a[While]
  }
  it should "parse a begin-end statement" in {
    statementParser.runParser("begin skip end").get shouldBe a[BeginEnd]
  }
  it should "parse a sequential composition statement" in {
    statementParser
      .runParser("int x = 0; x = x + 1; return x")
      .get shouldBe a[StatementSequence]
  }

  "An assign left parser" should "parse any identifier" in {
    assignmentLeftParser.runParser("ident").get shouldBe an[AssignmentLeft]
    assignmentLeftParser.runParser("_IdenT1").get shouldBe an[AssignmentLeft]
  }
  it should "parse any array element" in {
    assignmentLeftParser.runParser("array[10]").get shouldBe an[AssignmentLeft]
    assignmentLeftParser
      .runParser("array['a'][10]")
      .get shouldBe an[AssignmentLeft]
  }
  it should "parse any pair element" in {
    assignmentLeftParser.runParser("fst ident").get shouldBe an[AssignmentLeft]
    assignmentLeftParser
      .runParser("snd array[10][200]")
      .get shouldBe an[AssignmentLeft]
  }

  "An assign right parser" should "parse any expression" in {
    assignmentRightParser.runParser("1+2+(3*4)").get shouldBe a[AssignmentRight]
    assignmentRightParser.runParser("len(4%4)").get shouldBe a[AssignmentRight]
  }
  it should "parse any array literal" in {
    assignmentRightParser.runParser("[1]").get shouldBe a[AssignmentRight]
    assignmentRightParser
      .runParser("['a', \"aString\"]")
      .get shouldBe a[AssignmentRight]
  }
  it should "parse any newpair creation" in {
    assignmentRightParser
      .runParser("newpair(\"this\", \"that\")")
      .get shouldBe a[AssignmentRight]
    assignmentRightParser
      .runParser("newpair(1+1, ord('a')")
      .get shouldBe a[AssignmentRight]
  }
  it should "parse any pair element" in {
    assignmentRightParser.runParser("fst ident").get shouldBe a[AssignmentRight]
    assignmentRightParser
      .runParser("snd (a[100][20])")
      .get shouldBe a[AssignmentRight]
  }
  it should "parse any function call" in {
    assignmentRightParser
      .runParser("call fun()")
      .get shouldBe a[AssignmentRight]
    assignmentRightParser
      .runParser("call fun2(100, 1000)")
      .get shouldBe a[AssignmentRight]
  }

  "An argument list parser" should "parse any non-empty list of expressions" in {
    argumentListParser.runParser("100").get shouldBe an[ArgumentList]
    argumentListParser.runParser("100, 200").get shouldBe an[ArgumentList]
    argumentListParser.runParser("5+5, 7*3-(4+9)").get shouldBe an[ArgumentList]
    argumentListParser
      .runParser("chr(96), ord('x'), (len(ident))")
      .get shouldBe an[ArgumentList]
  }

  "A pair element parser" should "parse strings of the form ('fst' | 'snd') expression" in {
    pairElementParser.runParser("fst 1").get shouldBe an[PairElement]
    pairElementParser
      .runParser("snd (len(chr(ord(xyz))))")
      .get shouldBe an[PairElement]
  }

  "A type parser" should "parse all valid types" in {
    typeParser.runParser("int").get shouldBe an[Type]
    typeParser.runParser("bool[][]").get shouldBe an[Type]
    typeParser.runParser("pair(int, char)").get shouldBe an[Type]
    typeParser.runParser("pair(pair, string[][])[]").get shouldBe an[Type]
  }

  "A base type parser" should "parse all base types" in {
    baseTypeParser.runParser("int").get shouldBe an[BaseType]
    baseTypeParser.runParser("bool").get shouldBe an[BaseType]
    baseTypeParser.runParser("char").get shouldBe an[BaseType]
    baseTypeParser.runParser("string").get shouldBe an[BaseType]
  }

  "An array type parser" should "parse any array type" in {
    arrayTypeParser.runParser("int[]").get shouldBe an[ArrayType]
    arrayTypeParser.runParser("string[][]").get shouldBe an[ArrayType]
    arrayTypeParser.runParser("pair(int, int)[]").get shouldBe an[ArrayType]
    arrayTypeParser.runParser("pair(pair, int[])[]").get shouldBe an[ArrayType]
  }

  "A pair type parser" should "parse any valid pair type" in {
    pairTypeParser.runParser("pair(int, char)").get shouldBe an[PairType]
    pairTypeParser.runParser("pair(string, string)").get shouldBe an[PairType]
    pairTypeParser.runParser("pair(pair, pair)").get shouldBe an[PairType]
    pairTypeParser
      .runParser("pair(int[], string[][])")
      .get shouldBe an[PairType]
  }

  "A pair element type parser" should "parse any valid pair element type" in {
    pairElementTypeParser.runParser("int").get shouldBe an[PairElementType]
    pairElementTypeParser.runParser("int[]").get shouldBe an[PairElementType]
    pairElementTypeParser.runParser("pair").get shouldBe an[PairElementType]
  }

  "An expression parser" should "parse any literal" in {
    expressionParser.runParser("-100").get shouldBe an[Expression]
    expressionParser.runParser("true").get shouldBe an[Expression]
    expressionParser.runParser("'x'").get shouldBe an[Expression]
    expressionParser.runParser("\"string\"").get shouldBe an[Expression]
    expressionParser.runParser("null").get shouldBe an[Expression]
    expressionParser.runParser("identifier").get shouldBe an[Expression]
    expressionParser.runParser("array[10][10]").get shouldBe an[Expression]
  }
  it should "parse any unary operator application" in {
    expressionParser
      .runParser("!true")
      .get shouldBe an[UnaryOperatorApplication]
    expressionParser
      .runParser("--100")
      .get shouldBe an[UnaryOperatorApplication]
    expressionParser
      .runParser("len(\"thisstring\")")
      .get shouldBe an[UnaryOperatorApplication]
    expressionParser
      .runParser("ord('x')")
      .get shouldBe an[UnaryOperatorApplication]
    expressionParser
      .runParser("chr(100)")
      .get shouldBe an[UnaryOperatorApplication]
  }
  it should "parse any binary operator application" in {
    for (op <- BinaryOperator.operators) {
      expressionParser
        .runParser("5 " + op + " 5")
        .get shouldBe an[BinaryOperatorApplication]
    }
  }
  it should "parse any complex expression" in {
    expressionParser.runParser("(1 + 2) * (3 + 4)").get shouldBe an[Expression]
    expressionParser
      .runParser("len(\"this\") + chr(111)")
      .get shouldBe an[Expression]

    expressionParser
      .runParser("true && (false || (false && (ident)))")
      .get shouldBe an[Expression]
    expressionParser.runParser("(((((100)))))").get shouldBe an[Expression]
    expressionParser.runParser("((1 + 2)+len(3))").get shouldBe an[Expression]
  }

  "An unary operator parser" should "parse all unary operators" in {
    for (op <- UnaryOperator.operators) {
      println(op)
      unaryOperatorParser.runParser(op).get shouldBe an[UnaryOperator]
    }
  }
  it should "not parse anything else" in {
    unaryOperatorParser.runParser("xyz") shouldBe a[Failure]
    unaryOperatorParser.runParser("123") shouldBe a[Failure]
    unaryOperatorParser.runParser("\n ==") shouldBe a[Failure]
  }

  "A binary operator parser" should "parse all binary operators" in {
    for (op <- BinaryOperator.operators) {
      println(op)
      binaryOperatorParser.runParser(op).get shouldBe a[BinaryOperator]
    }
  }
  it should "not parse anything else" in {
    binaryOperatorParser.runParser("xyz") shouldBe a[Failure]
    binaryOperatorParser.runParser("123") shouldBe a[Failure]
    binaryOperatorParser.runParser("\n ==") shouldBe a[Failure]
  }

  "An identifier parser" should "parse valid one character strings" in {
    identifierParser.runParser("_").get shouldBe an[Identifier]
    identifierParser.runParser("x").get shouldBe an[Identifier]
    identifierParser.runParser("X").get shouldBe an[Identifier]
  }
  it should "parse any alphanumeric valid identifier string" in {
    identifierParser.runParser("aVariableName").get shouldBe an[Identifier]

    identifierParser
      .runParser("aNameContainingNumbers0123456789")
      .get shouldBe an[Identifier]
    identifierParser
      .runParser("_NameContainingUnderscore_")
      .get shouldBe an[Identifier]
    identifierParser
      .runParser("RandomLetters_34fsdf5JF9e7")
      .get shouldBe an[Identifier]
  }
  it should "fail to parse strings starting with digits" in {
    identifierParser.runParser("0name") shouldBe a[Failure]
    identifierParser.runParser("4name") shouldBe a[Failure]
    identifierParser.runParser("9name") shouldBe a[Failure]
  }
  it should "parse identifiers with keywords as substrings" in {
    identifierParser.runParser("truetrue").get shouldBe an[Identifier]
    identifierParser.runParser("while0").get shouldBe an[Identifier]
    identifierParser.runParser("xlen").get shouldBe an[Identifier]
  }

  "An array element parser" should "allow any array access of the form arrayName[expr1][expr2]...[exprN]" in {
    arrayElementParser.runParser("a[0]").get shouldBe an[ArrayElement]
    arrayElementParser.runParser("aName[100]").get shouldBe an[ArrayElement]
    arrayElementParser
      .runParser("arrayName[10+10]")
      .get shouldBe an[ArrayElement]
    arrayElementParser
      .runParser("a[100][100][100][100]")
      .get shouldBe an[ArrayElement]
    arrayElementParser
      .runParser("a[1+2+3][5-2][a]")
      .get shouldBe an[ArrayElement]
  }
  it should "fail to parse any badly constructed array access" in {
    arrayElementParser.runParser("a[]") shouldBe a[Failure]
    arrayElementParser.runParser("[]") shouldBe a[Failure]
    arrayElementParser.runParser("a[") shouldBe a[Failure]
    arrayElementParser.runParser("a[]]") shouldBe a[Failure]
    arrayElementParser.runParser("a[\n]") shouldBe a[Failure]
  }

  "An integer literal parser" should "parse any number (sequence of digits)" in {
    integerLiterParser.runParser("0").get shouldBe an[IntegerLiter]
    integerLiterParser.runParser("100").get shouldBe an[IntegerLiter]
    integerLiterParser.runParser("123456789").get shouldBe an[IntegerLiter]
  }
  it should "parse any number preceded by a '+' or '-'" in {
    integerLiterParser.runParser("+100").get shouldBe an[IntegerLiter]
    integerLiterParser.runParser("-100").get shouldBe an[IntegerLiter]
  }
  it should "fail to parse multiple signs" in {
    integerLiterParser.runParser("++100") shouldBe a[Failure]
    integerLiterParser.runParser("--100") shouldBe a[Failure]
  }

  "A digit parser" should "only parse digits" in {
    for (digit <- "0123456789") {
      digitParser.runParser(digit + "").get shouldBe an[Digit]
    }
    digitParser.runParser("x") shouldBe a[Failure]
  }

  "An integer sign parser" should "only parse a '+' or '-'" in {
    integerSignParser.runParser("+").get shouldBe an[IntegerSign]
    integerSignParser.runParser("-").get shouldBe an[IntegerSign]
    integerSignParser.runParser("x") shouldBe a[Failure]
    integerSignParser.runParser("5") shouldBe a[Failure]
  }

  "A boolean literal parser" should "only parse a 'true' or 'false' string" in {
    booleanLiterParser.runParser("true").get shouldBe a[BooleanLiter]
    booleanLiterParser.runParser("false").get shouldBe a[BooleanLiter]
    booleanLiterParser.runParser("xyz") shouldBe a[Failure]
    booleanLiterParser.runParser("tru") shouldBe a[Failure]
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
    characterLiterParser.runParser("\'x") shouldBe a[Failure]
    characterLiterParser.runParser("x\'") shouldBe a[Failure]
    characterLiterParser.runParser("x") shouldBe a[Failure]
    characterLiterParser.runParser("xyz") shouldBe a[Failure]
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
    stringLiterParser.runParser("\"No end quote") shouldBe a[Failure]
    stringLiterParser.runParser("No beggining quote\"") shouldBe a[Failure]
  }

  "A default character parser" should "parse any character except '\\', '\'' and '\"'" in {
    defaultCharacterParser.runParser("a").get shouldBe a[DefaultCharacter]
    defaultCharacterParser.runParser("Z").get shouldBe a[DefaultCharacter]
    defaultCharacterParser.runParser("9").get shouldBe a[DefaultCharacter]
    defaultCharacterParser.runParser("@").get shouldBe a[DefaultCharacter]
    defaultCharacterParser.runParser("[").get shouldBe a[DefaultCharacter]
  }
  it should "fail to parse '\\', '\'' and '\"'" in {
    defaultCharacterParser.runParser("\\") shouldBe a[Failure]
    defaultCharacterParser.runParser("\'") shouldBe a[Failure]
    defaultCharacterParser.runParser("\"") shouldBe a[Failure]
  }
  it should "only parse escapable characters after '\\'" in {
    for (chr <- EscapedCharacter.escapableCharacters) {
      defaultCharacterParser
        .runParser("\\" + chr)
        .get shouldBe a[DefaultCharacter]
    }
    defaultCharacterParser.runParser("\\a") shouldBe a[Failure]
  }

  "An escaped character parser" should "only parse escapable characters" in {
    for (chr <- EscapedCharacter.escapableCharacters) {
      val parsed = escapedCharParser.runParser(chr.toString)
      assert(parsed.isSuccess)
    }

    escapedCharParser.runParser("5") shouldBe a[Failure]
    escapedCharParser.runParser("a") shouldBe a[Failure]
    escapedCharParser.runParser("X") shouldBe a[Failure]
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

    parsed2 shouldBe a[Failure]
    parsed3 shouldBe a[Failure]
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
    val parsed = commentParser.runParser("# This is a comment \n")
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
