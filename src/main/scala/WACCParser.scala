import com.wacc
import com.wacc._
import com.wacc.operator.{BinaryOperator, UnaryOperator}
import parsley.Parsley._
import parsley.character._
import parsley.combinator.{attemptChoice, eof, manyN, option}
import parsley.expr._
import parsley.implicits.{voidImplicitly => _, _}
import parsley.{Parsley, combinator}

object WACCParser {
  /* 〈program〉::=  ‘begin’〈func〉*〈stat〉‘end’ */
  lazy val programParser: Parsley[Program] =
    Program.build
      .lift(
        skipWhitespace *> "begin" *> skipWhitespace *> combinator.many(
          attempt(functionParser)
        ),
        statementParser <* "end" <* skipWhitespace <* eof
      )
      .label("a program")
  /* 〈func〉::=〈type〉 〈ident〉‘(’〈param-list〉?  ‘)’ ‘is’〈stat〉‘end’ */
  lazy val functionParser: Parsley[wacc.Function] = Function.build
    .lift(
      typeParser,
      identifierParser,
      '(' *> skipWhitespace *> option(
        parameterListParser
      ) <* ')' <* skipWhitespace,
      "is" *> skipWhitespace *> statementParser <* "end" <* skipWhitespace
    )
    .label("a function definition")
  /* 〈param-list〉::=〈param〉( ‘,’〈param〉)* */
  lazy val parameterListParser: Parsley[ParameterList] =
    ParameterList.build
      .lift(
        parameterParser,
        combinator.many(',' *> skipWhitespace *> parameterParser)
      )
      .label("a parameter list")
  /* 〈param〉::=〈type〉 〈ident〉 */
  lazy val parameterParser: Parsley[Parameter] =
    Parameter.build.lift(typeParser, identifierParser).label("a parameter")
  /* 〈stat〉::=  ‘skip’
               | 〈type〉 〈ident〉‘=’〈assign-rhs〉
               | 〈assign-lhs〉‘=’〈assign-rhs〉
               | ‘read’〈assign-lhs〉
               | ‘free’〈expr〉
               | ‘return’〈expr〉
               | ‘exit’〈expr〉
               | ‘print’〈expr〉
               | ‘println’〈expr〉
               | ’if’〈expr〉‘then’〈stat〉‘else’〈stat〉‘fi’
               | ‘while’〈expr〉‘do’〈stat〉‘done’
               | ‘begin’〈stat〉‘end’
               | 〈stat〉‘;’〈stat〉*/
  lazy val statementParser: Parsley[Statement] = (precedence[Statement](
    (SkipStatement.build.lift("skip") <* skipWhitespace)
      <\> IdentifierDeclaration.build
        .lift(
          typeParser,
          identifierParser,
          "=" *> skipWhitespace *> assignmentRightParser
        )
      <\> Assignment.build
        .lift(
          assignmentLeftParser,
          '=' *> skipWhitespace *> assignmentRightParser
        )
      <\> Read.build.lift("read" *> skipWhitespace *> assignmentLeftParser)
      <\> Statement.buildActionExpression.lift(
        attemptChoice(
          "free",
          "return",
          "exit",
          "println",
          "print"
        ) <* skipWhitespace,
        expressionParser
      )
      <\> If.build.lift(
        "if" *> skipWhitespace *> expressionParser,
        "then" *> skipWhitespace *> statementParser,
        "else" *> skipWhitespace *> statementParser <* "fi" <* skipWhitespace
      )
      <\> While.build
        .lift(
          "while" *> skipWhitespace *> expressionParser,
          "do" *> skipWhitespace *> statementParser <* "done" <* skipWhitespace
        )
      <\> BeginEnd.build.lift(
        "begin" *> skipWhitespace *> statementParser <* "end" <* skipWhitespace
      ),
    Ops(InfixL)((";" <* skipWhitespace) #> StatementSequence.build)
  ) <* skipWhitespace)
  /* 〈assign-lhs〉::=〈ident〉
                   | 〈array-elem〉
                   | 〈pair-elem〉*/
  lazy val assignmentLeftParser: Parsley[AssignmentLeft] =
    ((pairElementParser <\> arrayElementParser <\> identifierParser) <* skipWhitespace)
      .label("a left assignment")
  /* 〈assign-rhs〉::=〈expr〉
                   | 〈array-liter〉
                   | ‘newpair’ ‘(’〈expr〉‘,’〈expr〉‘)’
                   | 〈pair-elem〉
                   | ‘call’〈ident〉‘(’〈arg-list〉?  ‘)’ */
  lazy val assignmentRightParser: Parsley[AssignmentRight] =
    (newpairParser <\> functionCallParser <\> pairElementParser <\> expressionParser <\> arrayLiterParser) <* skipWhitespace
  lazy val newpairParser: Parsley[NewPair] =
    NewPair.build
      .lift(
        "newpair" *> skipWhitespace *> "(" *> skipWhitespace *> expressionParser,
        ',' *> skipWhitespace *> expressionParser <* ')' <* skipWhitespace
      )
      .label("a newpair initialization")
  lazy val functionCallParser: Parsley[FunctionCall] =
    FunctionCall.build
      .lift(
        "call" *> skipWhitespace *> identifierParser,
        '(' *> skipWhitespace *> option(argumentListParser) <* ')'
      )
      .label("a function call")
  /*〈arg-list〉::=〈expr〉(‘,’〈expr〉)* */
  lazy val argumentListParser: Parsley[ArgumentList] =
    ArgumentList.build
      .lift(
        expressionParser,
        combinator.many(',' *> skipWhitespace *> expressionParser)
      )
      .label("an argument list")
  /* 〈pair-elem〉::= ‘fst’〈expr〉
                    |‘snd’〈expr〉 */
  lazy val pairElementParser: Parsley[PairElement] =
    (PairElement.build.lift(
      "fst" *> skipWhitespace *> expressionParser,
      unit.map(_ => true)
    ) <\>
      PairElement.build.lift(
        "snd" *> skipWhitespace *> expressionParser,
        unit.map(_ => false)
      )).label("a pair element")
  /* 〈type〉::=〈base-type〉
             | 〈array-type〉
             | 〈pair-type〉 */
  lazy val typeParser: Parsley[Type] = (precedence[Type](
    pairTypeParser <\> baseTypeParser,
    Ops(Postfix)("[]" #> toArrayType)
  ) <* skipWhitespace).label("a type")
  lazy val toArrayType: Type => ArrayType = ArrayType(_)
  /* 〈base-type〉::= ‘int’
                   | ‘bool’
                   | ‘char’
                   | ‘string’ */
  lazy val baseTypeParser: Parsley[BaseType] =
    (BaseType.build.lift(
      attemptChoice(BaseType.types.map(attempt(_)): _*)
    ) <* skipWhitespace).label("a base type")
  /*〈array-type〉::=〈type〉‘[’ ‘]’ */
  lazy val arrayTypeParser: Parsley[ArrayType] =
    (ArrayType.build.lift(
      lookAhead(
        attemptChoice(baseTypeParser, pairTypeParser) *> "["
      ) *> typeParser
    ) <* skipWhitespace).label("an array type")
  /*〈pair-type〉::=  ‘pair’ ‘(’〈pair-elem-type〉‘,’〈pair-elem-type〉‘)’ */
  lazy val pairTypeParser: Parsley[PairType] =
    (PairType.build.lift(
      "pair" *> skipWhitespace *> "(" *> skipWhitespace *> pairElementTypeParser,
      "," *> skipWhitespace *> pairElementTypeParser <* ")"
    ) <* skipWhitespace).label("a pair type")
  /* 〈pair-elem-type〉::=〈base-type〉
                       | ‘pair’
                       | 〈array-type〉 */
  lazy val pairElementTypeParser: Parsley[PairElementType] =
    ((PairDefault.build.lift(
      "pair"
    ) <\> (arrayTypeParser) <\> baseTypeParser) <* skipWhitespace)
      .label("a pair element type")
  lazy val expressionParser: Parsley[Expression] = (precedence[Expression](
    attempt(
      "(" *> skipWhitespace *> expressionParser <* skipWhitespace <* ")" <* skipWhitespace
    )
      <\> attempt(integerLiterParser)
      <\> attempt(booleanLiterParser)
      <\> attempt(characterLiterParser)
      <\> attempt(stringLiterParser)
      <\> attempt(pairLiterParser)
      <\> attempt(arrayElementParser)
      <\> attempt(identifierParser),
    Ops(Prefix)(
      attempt(
        ("!".label(
          "an unary operator"
        ) <* skipWhitespace) #> unaryFunctionGenerator("!")
      ),
      attempt(
        ("-".label(
          "an unary operator"
        ) <* skipWhitespace) #> unaryFunctionGenerator("-")
      ),
      attempt(
        ("len".label(
          "an unary operator"
        ) <* skipWhitespace) #> unaryFunctionGenerator("len")
      ),
      attempt(
        ("ord".label(
          "an unary operator"
        ) <* skipWhitespace) #> unaryFunctionGenerator("ord")
      ),
      attempt(
        ("chr".label(
          "an unary operator"
        ) <* skipWhitespace) #> unaryFunctionGenerator("chr")
      )
    ),
    Ops(InfixL)(
      ("*".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("*"),
      ("/".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("/"),
      ("%".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("%")
    ),
    Ops(InfixL)(
      ("+".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("+"),
      ("-".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("-")
    ),
    Ops(InfixL)(
      (attempt(">=").label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator(">="),
      (attempt("<=").label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("<="),
      (attempt("==").label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("=="),
      ("!=".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("!="),
      (">".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator(">"),
      ("<".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("<")
    ),
    Ops(InfixL)(
      ("&&".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("&&"),
      ("||".label(
        "a binary operator"
      ) <* skipWhitespace) #> binaryFunctionGenerator("||")
    )
  ) <* skipWhitespace).label("an expression")

  /*〈expr〉::=〈int-liter〉
            | 〈bool-liter〉
            | 〈char-liter〉
            | 〈str-liter〉
            | 〈pair-liter〉
            | 〈ident〉
            | 〈array-elem〉
            | 〈unary-oper〉 〈expr〉
            | 〈expr〉 〈binary-oper〉 〈expr〉
            | ‘(’〈expr〉‘)’ */
  lazy val unaryFunctionGenerator
      : String => Expression => UnaryOperatorApplication =
    (operator: String) =>
      (expr: Expression) =>
        UnaryOperatorApplication(UnaryOperator(operator), expr)
  lazy val binaryFunctionGenerator
      : String => (Expression, Expression) => BinaryOperatorApplication =
    (operator: String) =>
      (expr1: Expression, expr2: Expression) =>
        BinaryOperatorApplication(expr1, BinaryOperator(operator), expr2)
  /* 〈unary-oper〉::= ‘!’
                    | ‘-’
                    | ‘len’
                    | ‘ord’
                    | ‘chr’ */
  lazy val unaryOperatorParser: Parsley[UnaryOperator] =
    UnaryOperator.build
      .lift(
        attemptChoice(UnaryOperator.operators.map(attempt(_)): _*)
      )
      .label("an unary operator")
  /*〈binary-oper〉::= ‘*’
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
    BinaryOperator.build
      .lift(
        attemptChoice(
          BinaryOperator.operators.map(attempt(_)): _*
        ) <* skipWhitespace
      )
      .label("a binary operator")
  /*〈ident〉::=  ( ‘’|‘a’-‘z’|‘A’-‘Z’ ) ( ‘’|‘a’-‘z’|‘A’-‘Z’|‘0’-‘9’ )* */
  lazy val identifierParser: Parsley[Identifier] =
    Identifier.buildKeywordPrefix
      .lift(
        attemptChoice(keywords.map(attempt(_)): _*),
        combinator
          .manyN(1, '_' <\> alphaNum) <* skipWhitespace
      )
      .label("an identifier") <|> Identifier.build
      .lift(
        '_' <\> letter,
        combinator.many('_' <\> alphaNum) <* skipWhitespace
      )
      .label("an identifier")
  /*〈array-elem〉::=〈ident〉(‘[’〈expr〉‘]’)+ */
  lazy val arrayElementParser: Parsley[ArrayElement] =
    ArrayElement.build
      .lift(
        identifierParser,
        manyN(
          1,
          "[" *> skipWhitespace *> expressionParser <* "]"
        ) <* skipWhitespace
      )
      .label("an array element")
  /* 〈int-liter〉::=〈int-sign〉?〈digit〉+ */
  lazy val integerLiterParser: Parsley[IntegerLiter] =
    IntegerLiter.build
      .lift(
        option(integerSignParser),
        manyN(1, digitParser) <* skipWhitespace
      )
      .label("an integer")
  /*〈digit〉::=  (‘0’-‘9’) */
  lazy val digitParser: Parsley[Digit] =
    Digit.build.lift(satisfy(Digit.digits.contains(_))).label("a digit")
  /*〈int-sign〉::=  ‘+’|‘-’ */
  lazy val integerSignParser: Parsley[IntegerSign] =
    IntegerSign.build.lift('+' <\> '-')
  /*〈bool-liter〉::=  ‘true’|‘false’ */
  lazy val booleanLiterParser: Parsley[BooleanLiter] =
    BooleanLiter.build
      .lift(("true" <\> "false") <* skipWhitespace)
      .label("a boolean")
  /*〈char-liter〉::=  ‘'’〈character〉‘'’ */
  lazy val characterLiterParser: Parsley[CharacterLiter] =
    CharacterLiter.build
      .lift(
        "\'" *> defaultCharacterParser <* "\'" <* skipWhitespace
      )
      .label("a character literal")
  /*〈str-liter〉::=  ‘"’〈character〉* ‘"’ */
  lazy val stringLiterParser: Parsley[StringLiter] = StringLiter.build
    .lift(
      "\"" *> combinator.many(defaultCharacterParser) <* "\"" <* skipWhitespace
    )
    .label("a string literal")
  /*〈character〉::= any-ASCII-character-except-‘\’-‘'’-‘"’
                  | ‘\’〈escaped-char〉*/
  lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
    DefaultCharacter.build
      .lift(noneOf('\\', '\'', '\"'), "" #> false)
      .label("a character different than \\, \' and \"") <\>
      DefaultCharacter.build.lift(
        ('\\' *> escapedCharParser).map(esc => esc.char),
        "" #> true
      )
  /*〈escaped-char〉::= ‘0’
                     | ‘b’
                     | ‘t’
                     | ‘n’
                     | ‘f’
                     | ‘r’
                     | ‘"’
                     | ‘'’
                     | ‘\’ */
  lazy val escapedCharParser: Parsley[EscapedCharacter] =
    EscapedCharacter.build
      .lift(
        satisfy(EscapedCharacter.escapableCharacters.contains(_))
      )
      .label("an escapable character")
  /*〈array-liter〉::= ‘[’ (〈expr〉(‘,’〈expr〉)* )?  ‘]’ */
  lazy val arrayLiterParser: Parsley[ArrayLiter] = ArrayLiter.build
    .lift(
      "[" *> skipWhitespace *> option(
        expressionParser <~> combinator.many(
          "," *> skipWhitespace *> expressionParser
        )
      ) <* "]" <* skipWhitespace
    )
    .label("an array literal")
  /*〈pair-liter〉::=  ‘null’ */
  lazy val pairLiterParser: Parsley[PairLiter] =
    PairLiter.build.lift("null" <* skipWhitespace).label("null")
  /* 〈comment〉::=  ‘#’ (any-character-except-EOL)*〈EOL〉 */
  lazy val commentParser: Parsley[Comment] =
    Comment.build.lift('#' *> combinator.manyUntil(anyChar, "\n")).hide
  lazy val skipWhitespace: Parsley[Unit] =
    combinator.skipMany(whitespace.hide <\> commentParser).hide
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
    "false"
  )
}
