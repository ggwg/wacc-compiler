import com.wacc
import com.wacc._
import com.wacc.operator.{BinaryOperator, UnaryOperator}
import parsley.Parsley._
import parsley.character._
import parsley.combinator.{attemptChoice, eof, manyN, option}
import parsley.expr._
import parsley.implicits.{voidImplicitly => _, _}
import parsley.{Parsley, combinator}

object Parser {
  /*  <program> ::=  ‘begin’ <import>* <func>* <stat> ‘end’ */
  lazy val programParser: Parsley[Program] =
    Program(
      skipWhitespace *> parseKeyword("begin") *> skipWhitespace *>
        combinator.many(attempt(importParser)),
      combinator.many(attempt(functionParser)),
      statementParser <* parseKeyword("end") <* skipWhitespace <* eof
    )
      .label("a program")
  /* <import> ::= (‘import‘ <identifier>)* */
  lazy val importParser: Parsley[Import] =
    Import(parseKeyword("import") *> skipWhitespace *> identifierParser <* skipWhitespace)
      .label("an import")
  /*  <func> ::= <type>   <ident> ‘(’ <param-list> ?  ‘)’ ‘is’ <stat> ‘end’ */
  lazy val functionParser: Parsley[wacc.Function] = Function(
    typeParser,
    identifierParser,
    '(' *> skipWhitespace *> option(parameterListParser) <* ')' <* skipWhitespace,
    parseKeyword("is") *> skipWhitespace *> statementParser <* parseKeyword("end") <* skipWhitespace
  )
    .label("a function definition")
  /*  <param-list> ::= <param> ( ‘,’ <param> )* */
  lazy val parameterListParser: Parsley[ParameterList] =
    ParameterList(parameterParser, combinator.many(',' *> skipWhitespace *> parameterParser))
      .label("a parameter list")
  /*  <param> ::= <type>   <ident>  */
  lazy val parameterParser: Parsley[Parameter] =
    Parameter(typeParser, identifierParser).label("a parameter")
  /*  <stat> ::=  ‘skip’
               |  <type>   <ident> ‘=’ <assign-rhs>
               |  <assign-lhs> ‘=’ <assign-rhs>
               | ‘read’ <assign-lhs>
               | ‘free’ <expr>
               | ‘return’ <expr>
               | ‘exit’ <expr>
               | ‘print’ <expr>
               | ‘println’ <expr>
               | ’if’ <expr> ‘then’ <stat> ‘else’ <stat> ‘fi’
               | ‘while’ <expr> ‘do’ <stat> ‘done’
               | ‘for‘ ‘(‘ <initialization>* <expr>? <assignment>* ‘)‘ ‘do‘ <stat> ‘done‘
               | ‘break‘
               | ‘continueloop‘
               | ‘try‘ <stat> ‘catch‘ <stat> ‘end‘
               | ‘begin’ <stat> ‘end’
               |  <stat> ‘;’ <stat> */
  lazy val statementParser: Parsley[Statement] = precedence[Statement](
    (SkipStatement(parseKeyword("skip")) <* skipWhitespace)
      <\> (Break(parseKeyword("break")) <* skipWhitespace)
      <\> (ContinueLoop(parseKeyword("continueloop")) <* skipWhitespace)
      <\> identifierDeclarationParser
      <\> assignmentParser
      <\> Read(parseKeyword("read") *> skipWhitespace *> assignmentLeftParser)
      <\> Statement(
        attemptChoice(
          parseKeyword("free"),
          parseKeyword("return"),
          parseKeyword("exit"),
          parseKeyword("println"),
          parseKeyword("print")
        ) <* skipWhitespace,
        expressionParser
      )
      <\> StatementFunctionCall(functionCallParser) <* skipWhitespace
      <\> If(
        parseKeyword("if") *> skipWhitespace *> expressionParser,
        parseKeyword("then") *> skipWhitespace *> statementParser,
        parseKeyword("else") *> skipWhitespace *> statementParser <* parseKeyword("fi") <* skipWhitespace
      )
      <\> TryCatch(
        parseKeyword("try") *> skipWhitespace *> statementParser,
        parseKeyword("catch") *> skipWhitespace *> statementParser <* parseKeyword("end") <* skipWhitespace
      )
      <\> While(
        parseKeyword("while") *> skipWhitespace *> expressionParser,
        parseKeyword("do") *> skipWhitespace *> statementParser <* parseKeyword("done") <* skipWhitespace
      )
      <\> For(
        parseKeyword("for") *> skipWhitespace *> '(' *> skipWhitespace *> option(
          initializationListParser
        ) <* ';' <* skipWhitespace,
        option(expressionParser) <* ';' <* skipWhitespace,
        option(assignmentListParser) <* ')' <* skipWhitespace,
        parseKeyword("do") *> skipWhitespace *> statementParser <* parseKeyword("done") <* skipWhitespace
      )
      <\> BeginEnd(parseKeyword("begin") *> skipWhitespace *> statementParser <* parseKeyword("end") <* skipWhitespace),
    Ops(InfixL)(
      (";" <* skipWhitespace) #> ((st1: Statement, st2: Statement) => StatementSequence(st1, st2)(st1.getPos()))
    )
  ) <* skipWhitespace
  /*  <assign-lhs> ::= <ident>
                   |  <array-elem>
                   |  <pair-elem> */

  lazy val assignmentParser: Parsley[Assignment] =
    Assignment(assignmentLeftParser, '=' *> skipWhitespace *> assignmentRightParser)

  lazy val identifierDeclarationParser: Parsley[IdentifierDeclaration] =
    IdentifierDeclaration(typeParser, identifierParser, "=" *> skipWhitespace *> assignmentRightParser)

  lazy val initializationParser: Parsley[Initialization] =
    (identifierDeclarationParser <\> assignmentParser)

  lazy val initializationListParser: Parsley[List[Initialization]] =
    Initialization(initializationParser, combinator.many(',' *> skipWhitespace *> initializationParser))

  lazy val assignmentListParser: Parsley[List[Assignment]] =
    Assignment.parsleyList(assignmentParser, combinator.many(',' *> skipWhitespace *> assignmentParser))

  /* 〈assign-lhs〉::=〈ident〉
                   | 〈array-elem〉
                   | 〈pair-elem〉*/
  lazy val assignmentLeftParser: Parsley[AssignmentLeft] =
    ((pairElementParser <\> arrayElementParser <\> identifierParser) <* skipWhitespace)
      .label("a left assignment")
  /*  <assign-rhs> ::= <expr>
                   |  <array-liter>
                   | ‘newpair’ ‘(’ <expr> ‘,’ <expr> ‘)’
                   |  <pair-elem>
                   | ‘call’ <ident> ‘(’ <arg-list> ?  ‘)’ */
  lazy val assignmentRightParser: Parsley[AssignmentRight] =
    (newpairParser <\> functionCallParser <\> pairElementParser <\> expressionParser <\> arrayLiterParser) <* skipWhitespace
  lazy val newpairParser: Parsley[NewPair] =
    NewPair(
      parseKeyword("newpair") *> skipWhitespace *> "(" *> skipWhitespace *> expressionParser,
      ',' *> skipWhitespace *> expressionParser <* ')' <* skipWhitespace
    )
      .label("a newpair initialization")
  lazy val functionCallParser: Parsley[FunctionCall] =
    FunctionCall(
      parseKeyword("call") *> skipWhitespace *> identifierParser,
      '(' *> skipWhitespace *> option(argumentListParser) <* ')'
    )
      .label("a function call")
  /* <arg-list> ::= <expr> (‘,’ <expr> )* */
  lazy val argumentListParser: Parsley[ArgumentList] =
    ArgumentList(expressionParser, combinator.many(',' *> skipWhitespace *> expressionParser))
      .label("an argument list")
  /*  <pair-elem> ::= ‘fst’ <expr>
                    |‘snd’ <expr>  */
  lazy val pairElementParser: Parsley[PairElement] =
    (PairElement(parseKeyword("fst") *> skipWhitespace *> expressionParser, isFirst = true) <\>
      PairElement(parseKeyword("snd") *> skipWhitespace *> expressionParser, isFirst = false)).label("a pair element")
  /*  <type> ::= <base-type>
             |  <array-type>
             |  <pair-type>
             |  <func-type>
             |  <void-type> */
  lazy val typeParser: Parsley[Type] =
    (precedence[Type](
      voidTypeParser <\> pairTypeParser <\> baseTypeParser <\> functionTypeParser,
      Ops(Postfix)("[]" #> toArrayType)
    ) <* skipWhitespace)
      .label("a type")
  lazy val toArrayType: Type => ArrayType = ArrayType(_)
  /*  <base-type> ::= ‘int’
                   | ‘bool’
                   | ‘char’
                   | ‘string’ */
  lazy val voidTypeParser: Parsley[VoidType] =
    (VoidType(parseKeyword("void")) <* skipWhitespace)
      .label("a void type")
  lazy val baseTypeParser: Parsley[BaseType] =
    (BaseType(attemptChoice(BaseType.types.map(parseKeyword(_)): _*)) <* skipWhitespace).label("a base type")
  /* <array-type> ::= <type> ‘[’ ‘]’ */
  lazy val arrayTypeParser: Parsley[ArrayType] =
    (lookAhead(attemptChoice(baseTypeParser, pairTypeParser, functionTypeParser) *> "[") *> typeParser)
      .map(_.asInstanceOf[ArrayType]) <* skipWhitespace
      .label("an array type")
  /* <func-type> ::= 'func' '(' <type>, <type>* ')' */
  lazy val functionTypeParser: Parsley[FunctionType] = (FunctionType(
    parseKeyword("func") *> skipWhitespace *> "(" *> typeParser,
    option[List[Type]](combinator.many("," *> skipWhitespace *> typeParser)) <* ")"
  ) <* skipWhitespace).label("a function type")

  /* <pair-type> ::=  ‘pair’ ‘(’ <pair-elem-type> ‘,’ <pair-elem-type> ‘)’ */
  lazy val pairTypeParser: Parsley[PairType] =
    (PairType(
      parseKeyword("pair") *> skipWhitespace *> "(" *> skipWhitespace *> pairElementTypeParser,
      "," *> skipWhitespace *> pairElementTypeParser <* ")"
    ) <* skipWhitespace).label("a pair type")
  /*  <pair-elem-type> ::= <base-type>
                       | ‘pair’
                       |  <array-type>  */
  lazy val pairElementTypeParser: Parsley[PairElementType] =
    ((PairDefault(parseKeyword("pair")) <\> arrayTypeParser <\> baseTypeParser) <* skipWhitespace)
      .label("a pair element type")
  lazy val expressionParser: Parsley[Expression] = (precedence[Expression](
    attempt("(" *> skipWhitespace *> expressionParser <* skipWhitespace <* ")" <* skipWhitespace)
      <\> attempt(integerLiterParser)
      <\> attempt(booleanLiterParser)
      <\> attempt(characterLiterParser)
      <\> attempt(stringLiterParser)
      <\> attempt(pairLiterParser)
      <\> attempt(arrayElementParser)
      <\> attempt(identifierParser),
    Ops(Prefix)(
      attempt(("++".label("an unary operator") <* skipWhitespace) #> incDecFunctionGenerator(true, true)),
      attempt(("--".label("an unary operator") <* skipWhitespace) #> incDecFunctionGenerator(true, false)),
      attempt(("!".label("an unary operator") <* skipWhitespace) #> unaryFunctionGenerator("!")),
      attempt(("-".label("an unary operator") <* skipWhitespace) #> unaryFunctionGenerator("-")),
      attempt((parseKeyword("len").label("an unary operator") <* skipWhitespace) #> unaryFunctionGenerator("len")),
      attempt((parseKeyword("ord").label("an unary operator") <* skipWhitespace) #> unaryFunctionGenerator("ord")),
      attempt((parseKeyword("chr").label("an unary operator") <* skipWhitespace) #> unaryFunctionGenerator("chr"))
    ),
    Ops(InfixL)(
      ("*".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("*"),
      ("/".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("/"),
      ("%".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("%")
    ),
    Ops(InfixL)(
      ("+".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("+"),
      ("-".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("-")
    ),
    Ops(InfixL)(
      (attempt("<<").label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("<<"),
      (attempt(">>").label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator(">>")
    ),
    Ops(InfixL)(
      (attempt(">=").label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator(">="),
      (attempt("<=").label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("<="),
      (attempt("==").label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("=="),
      ("!=".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("!="),
      (">".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator(">"),
      ("<".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("<")
    ),
    Ops(InfixL)(
      (attempt("&" <* notFollowedBy("&")).label("A binary operator") <* skipWhitespace) #> binaryFunctionGenerator("&"),
      (attempt("|" <* notFollowedBy("|")).label("A binary operator") <* skipWhitespace) #> binaryFunctionGenerator("|")
    ),
    Ops(InfixL)(("&&".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("&&")),
    Ops(InfixL)(("||".label("a binary operator") <* skipWhitespace) #> binaryFunctionGenerator("||"))
  ) <* skipWhitespace).label("an expression")

  /* <expr> ::= <int-liter>
            |  <bool-liter>
            |  <char-liter>
            |  <str-liter>
            |  <pair-liter>
            |  <ident>
            |  <array-elem>
            |  <unary-oper>   <expr>
            |  <expr>   <binary-oper>   <expr>
            | ‘(’ <expr> ‘)’ */
  lazy val unaryFunctionGenerator: String => Expression => Expression =
    (operator: String) =>
      (expr: Expression) => {
        if (!operator.equals("-")) {
          UnaryOperatorApplication(UnaryOperator(operator), expr)(expr.getPos())
        } else
          expr match {
            case IntegerLiter(None, base, digits) =>
              IntegerLiter(Option(IntegerSign('-')), base, digits)(expr.getPos())
            case _ => UnaryOperatorApplication(UnaryOperator(operator), expr)(expr.getPos())
          }
      }
  lazy val incDecFunctionGenerator: (Boolean, Boolean) => Expression => Expression =
    (isPrefix: Boolean, isIncrement: Boolean) =>
      (expr: Expression) => IncDec(isPrefix, isIncrement, expr)(expr.getPos())

  lazy val binaryFunctionGenerator: String => (Expression, Expression) => BinaryOperatorApplication =
    (operator: String) =>
      (expr1: Expression, expr2: Expression) =>
        BinaryOperatorApplication(expr1, BinaryOperator(operator), expr2)(expr1.getPos())
  /*  <unary-oper> ::= ‘!’
                    | ‘-’
                    | ‘len’
                    | ‘ord’
                    | ‘chr’ */
  lazy val unaryOperatorParser: Parsley[UnaryOperator] =
    UnaryOperator(attemptChoice(UnaryOperator.operators.map(attempt(_)): _*))
      .label("an unary operator")
  /* <binary-oper> ::= ‘*’
                    | ‘/’
                    | ‘%’
                    | ‘+’
                    | ‘-’
                    | ‘>’
                    | ‘>=’
                    | ‘<’
                    | ‘<=’
                    | ‘==’
                    | ‘!=’
                    | ‘&&’
                    | ‘||’ */
  lazy val binaryOperatorParser: Parsley[BinaryOperator] =
    BinaryOperator(attemptChoice(BinaryOperator.operators.map(attempt(_)): _*) <* skipWhitespace)
      .label("a binary operator")
  /* <ident> ::=  ( ‘’|‘a’-‘z’|‘A’-‘Z’ ) ( ‘’|‘a’-‘z’|‘A’-‘Z’|‘0’-‘9’ )* */
  lazy val identifierParser: Parsley[Identifier] =
    Identifier(
      attemptChoice(keywords.map(attempt(_)): _*),
      combinator
        .manyN(1, '_' <\> alphaNum) <* skipWhitespace
    )
      .label("an identifier") <|> Identifier(
      ('_' <\> letter).map(c => c.toString),
      combinator.many('_' <\> alphaNum) <* skipWhitespace
    )
      .label("an identifier")
  /* <array-elem> ::= <ident> (‘[’ <expr> ‘]’)+ */
  lazy val arrayElementParser: Parsley[ArrayElement] =
    ArrayElement(identifierParser, manyN(1, "[" *> skipWhitespace *> expressionParser <* "]") <* skipWhitespace)
      .label("an array element")
  /*  <int-liter> ::= <int-sign> ? <digit> + */
  lazy val integerLiterParser: Parsley[IntegerLiter] =
    IntegerLiter(
      option(integerSignParser),
      option(attempt("0" *> oneOf('b', 'o', 'd', 'x'))),
      manyN(1, digitParser) <* skipWhitespace
    )
      .label("an integer")
  /* <digit> ::=  (‘0’-‘9’) */
  lazy val digitParser: Parsley[Digit] =
    Digit(satisfy(Digit.digits.contains(_))).label("a digit")
  /* <int-sign> ::=  ‘+’|‘-’ */
  lazy val integerSignParser: Parsley[IntegerSign] =
    IntegerSign('+' <\> '-')
  /* <bool-liter> ::=  ‘true’|‘false’ */
  lazy val booleanLiterParser: Parsley[BooleanLiter] =
    BooleanLiter((parseKeyword("true") <\> parseKeyword("false")) <* skipWhitespace)
      .label("a boolean")
  /* <char-liter> ::=  ‘'’ <character> ‘'’ */
  lazy val characterLiterParser: Parsley[CharacterLiter] =
    CharacterLiter("\'" *> defaultCharacterParser <* "\'" <* skipWhitespace)
      .label("a character literal")
  /* <str-liter> ::=  ‘"’ <character> * ‘"’ */
  lazy val stringLiterParser: Parsley[StringLiter] =
    StringLiter("\"" *> combinator.many(defaultCharacterParser) <* "\"" <* skipWhitespace)
      .label("a string literal")
  /* <character> ::= any-ASCII-character-except-‘\’-‘'’-‘"’
                  | ‘\’ <escaped-char> */
  lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
    DefaultCharacter(noneOf('\\', '\'', '\"'), isEscaped = false)
      .label("a character different than \\, \' and \"") <\>
      DefaultCharacter(('\\' *> escapedCharParser).map(esc => esc.char), isEscaped = true)
  /* <escaped-char> ::= ‘0’
                     | ‘b’
                     | ‘t’
                     | ‘n’
                     | ‘f’
                     | ‘r’
                     | ‘"’
                     | ‘'’
                     | ‘\’ */
  lazy val escapedCharParser: Parsley[EscapedCharacter] =
    EscapedCharacter(satisfy(EscapedCharacter.escapableCharacters.contains(_)))
      .label("an escapable character")
  /* <array-liter> ::= ‘[’ ( <expr> (‘,’ <expr> )* )?  ‘]’ */
  lazy val arrayLiterParser: Parsley[ArrayLiter] = ArrayLiter(
    "[" *> skipWhitespace *> option(
      expressionParser <~> combinator.many("," *> skipWhitespace *> expressionParser)
    ) <* "]" <* skipWhitespace
  )
    .label("an array literal")
  /* <pair-liter> ::=  ‘null’ */
  lazy val pairLiterParser: Parsley[PairLiter] =
    PairLiter(parseKeyword("null") <* skipWhitespace).label("null")
  /*  <comment> ::=  ‘#’ (any-character-except-EOL)* <EOL>  */
  lazy val commentParser: Parsley[Comment] =
    Comment('#' *> combinator.manyUntil(anyChar, "\n")).hide
  lazy val skipWhitespace: Parsley[Unit] =
    combinator.skipMany(whitespace.hide <\> commentParser).hide
  lazy val parseKeyword: String => Parsley[String] = keyword =>
    attempt(keyword) <* lookAhead(eof <|> satisfy(chr => !isAlphaNum(chr)))

  def isAlphaNum(chr: Char): Boolean = chr == '_' || (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z')

  val keywords = List(
    "begin",
    "end",
    "is",
    "skip",
    "read",
    "free",
    "return",
    "exit",
    "println",
    "print",
    "if",
    "then",
    "else",
    "fi",
    "while",
    "done",
    "do",
    "newpair",
    "call",
    "fst",
    "snd",
    "int",
    "bool",
    "char",
    "string",
    "pair",
    "len",
    "ord",
    "chr",
    "true",
    "false",
    "break",
    "continueloop",
    "for",
    "try",
    "catch"
  )
}
