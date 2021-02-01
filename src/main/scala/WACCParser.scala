import com.wacc
import com.wacc._
import com.wacc.operator.{BinaryOperator, UnaryOperator}
import parsley.Parsley._
import parsley.character.{alphaNum, anyChar, letter, noneOf, satisfy}
import parsley.combinator.{attemptChoice, manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits.{voidImplicitly => _, _}
import parsley.{Parsley, combinator}

object WACCParser {
  val keywords = List(
    "begin",
    "end",
    "is",
    "skip",
    "read",
    "free",
    "return",
    "exit",
    "print",
    "println",
    "if",
    "then",
    "else",
    "fi",
    "while",
    "do",
    "done",
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

  /* 〈program〉::=  ‘begin’〈func〉*〈stat〉‘end’ */
  lazy val programParser: Parsley[Program] =
    Program.build.lift(
      "begin" *> combinator.many(functionParser),
      statementParser <* "end"
    )

  /* 〈func〉::=〈type〉 〈ident〉‘(’〈param-list〉?  ‘)’ ‘is’〈stat〉‘end’ */
  lazy val functionParser: Parsley[wacc.Function] = Function.build.lift(
    typeParser,
    identifierParser,
    '(' *> option(parameterListParser) <* ')',
    "is" *> statementParser <* "end"
  )

  /* 〈param-list〉::=〈param〉( ‘,’〈param〉)* */
  lazy val parameterListParser: Parsley[ParameterList] =
    ParameterList.build.lift(
      parameterParser,
      combinator.many(',' *> parameterParser)
    )

  /* 〈param〉::=〈type〉 〈ident〉 */
  lazy val parameterParser: Parsley[Parameter] =
    Parameter.build.lift(typeParser, identifierParser)

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
  lazy val statementParser: Parsley[Statement] = precedence[Statement](
    SkipStatement.build.lift("skip")
      <\> IdentifierDeclaration.build
        .lift(typeParser, identifierParser, "=" *> assignmentRightParser)
      <\> Assignment.build
        .lift(assignmentLeftParser, '=' *> assignmentRightParser)
      <\> ("read" *> assignmentLeftParser).map(Read(_))
      <\> Statement.buildActionExpression.lift(
        attemptChoice("free", "return", "exit", "print", "println"),
        expressionParser
      )
      <\> If.build.lift(
        "if" *> expressionParser,
        "then" *> statementParser,
        "else" *> statementParser <* "fi"
      )
      <\> While.build
        .lift("while" *> expressionParser, "do" *> statementParser <* "done")
      <\> BeginEnd.build.lift("begin" *> statementParser <* "end"),
    Ops(InfixL)(";" #> StatementSequence.build)
  )

  /* 〈assign-lhs〉::=〈ident〉
                   | 〈array-elem〉
                   | 〈pair-elem〉*/
  lazy val assignmentLeftParser: Parsley[AssignmentLeft] =
    identifierParser <\> arrayElementParser <\> pairElementParser

  /* 〈assign-rhs〉::=〈expr〉
                   | 〈array-liter〉
                   | ‘newpair’ ‘(’〈expr〉‘,’〈expr〉‘)’
                   | 〈pair-elem〉
                   | ‘call’〈ident〉‘(’〈arg-list〉?  ‘)’ */
  lazy val assignmentRightParser: Parsley[AssignmentRight] =
    expressionParser <\> arrayLiterParser <\> newpairParser <\> pairElementParser <\> functionCallParser

  lazy val newpairParser: Parsley[NewPair] =
    NewPair.build.lift(
      "newpair(" *> expressionParser,
      ',' *> expressionParser <* ')'
    )

  lazy val functionCallParser: Parsley[FunctionCall] =
    FunctionCall.build.lift(
      "call" *> identifierParser,
      '(' *> option(argumentListParser) <* ')'
    )

  /*〈arg-list〉::=〈expr〉(‘,’〈expr〉)* */
  lazy val argumentListParser: Parsley[ArgumentList] =
    ArgumentList.build.lift(
      expressionParser,
      combinator.many(',' *> expressionParser)
    )

  /* 〈pair-elem〉::= ‘fst’〈expr〉
                    |‘snd’〈expr〉 */
  lazy val pairElementParser: Parsley[PairElement] = {
    PairElement.build.lift("fst" *> expressionParser, "" #> true) <\>
      PairElement.build.lift("snd" *> expressionParser, "" #> false)
  }

  /* 〈type〉::=〈base-type〉
             | 〈array-type〉
             | 〈pair-type〉 */
  lazy val typeParser: Parsley[Type] =
    baseTypeParser <\> arrayTypeParser <\> pairTypeParser

  /* 〈base-type〉::= ‘int’
                   | ‘bool’
                   | ‘char’
                   | ‘string’ */
  lazy val baseTypeParser: Parsley[BaseType] =
    BaseType.build.lift(attemptChoice(BaseType.types.map(attempt(_)): _*))

  /*〈array-type〉::=〈type〉‘[’ ‘]’ */
  lazy val arrayTypeParser: Parsley[ArrayType] =
    ArrayType.build.lift(typeParser <* '[' <* ']')

  /*〈pair-type〉::=  ‘pair’ ‘(’〈pair-elem-type〉‘,’〈pair-elem-type〉‘)’ */
  lazy val pairTypeParser: Parsley[PairType] =
    PairType.build.lift(
      "pair(" *> pairElementTypeParser,
      "," *> pairElementTypeParser <* ")"
    )

  /* 〈pair-elem-type〉::=〈base-type〉
                       | 〈array-type〉
                       | ‘pair’ */
  lazy val pairElementTypeParser: Parsley[PairElementType] =
    baseTypeParser <\> arrayTypeParser <\> PairDefault.build.lift("pair")

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

  lazy val expressionParser: Parsley[Expression] = precedence[Expression](
    integerLiterParser
      <\> booleanLiterParser
      <\> characterLiterParser
      <\> stringLiterParser
      <\> pairLiterParser
      <\> identifierParser
      <\> arrayElementParser
      <\> unaryOperatorApplicationParser
      <\> ("(" *> expressionParser <* ")"),
    Ops(InfixL)(
      "*" #> binaryFunctionGenerator("*"),
      "/" #> binaryFunctionGenerator("/"),
      "%" #> binaryFunctionGenerator("%")
    ),
    Ops(InfixL)(
      "+" #> binaryFunctionGenerator("+"),
      "-" #> binaryFunctionGenerator("-")
    ),
    Ops(InfixL)(
      ">" #> binaryFunctionGenerator(">"),
      ">=" #> binaryFunctionGenerator(">="),
      "<" #> binaryFunctionGenerator("<"),
      "<=" #> binaryFunctionGenerator("<="),
      "==" #> binaryFunctionGenerator("=="),
      "!=" #> binaryFunctionGenerator("!=")
    ),
    Ops(InfixL)(
      "&" #> binaryFunctionGenerator("+"),
      "-" #> binaryFunctionGenerator("-")
    )
  )

  lazy val unaryOperatorApplicationParser: Parsley[UnaryOperatorApplication] =
    UnaryOperatorApplication.build.lift(unaryOperatorParser, expressionParser)

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
    UnaryOperator.build.lift(
      attemptChoice(UnaryOperator.operators.map(attempt(_)): _*)
    )

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
    BinaryOperator.build.lift(
      attemptChoice(BinaryOperator.operators.map(attempt(_)): _*)
    )

  /*〈ident〉::=  ( ‘’|‘a’-‘z’|‘A’-‘Z’ ) ( ‘’|‘a’-‘z’|‘A’-‘Z’|‘0’-‘9’ )* */
  lazy val identifierParser: Parsley[Identifier] =
    Identifier.build.lift('_' <\> letter, combinator.many('_' <\> alphaNum))

  /*〈array-elem〉::=〈ident〉(‘[’〈expr〉‘]’)+ */
  lazy val arrayElementParser: Parsley[ArrayElement] =
    ArrayElement.build.lift(
      identifierParser,
      manyN(1, "[" *> expressionParser <* "]")
    )

  /* 〈int-liter〉::=〈int-sign〉?〈digit〉+ */
  lazy val integerLiterParser: Parsley[IntegerLiter] =
    IntegerLiter.build.lift(option(integerSignParser), manyN(1, digitParser))

  /*〈digit〉::=  (‘0’-‘9’) */
  lazy val digitParser: Parsley[Digit] =
    Digit.build.lift(satisfy(Digit.digits.contains(_)))

  /*〈int-sign〉::=  ‘+’|‘-’ */
  lazy val integerSignParser: Parsley[IntegerSign] =
    IntegerSign.build.lift('+' <\> '-')

  /*〈bool-liter〉::=  ‘true’|‘false’ */
  lazy val booleanLiterParser: Parsley[BooleanLiter] =
    BooleanLiter.build.lift("true" <\> "false")

  /*〈char-liter〉::=  ‘'’〈character〉‘'’ */
  lazy val characterLiterParser: Parsley[CharacterLiter] =
    CharacterLiter.build.lift("\'" *> defaultCharacterParser <* "\'")

  /*〈str-liter〉::=  ‘"’〈character〉* ‘"’ */
  lazy val stringLiterParser: Parsley[StringLiter] = StringLiter.build.lift(
    "\"" *> combinator.many(defaultCharacterParser) <* "\""
  )

  /*〈character〉::= any-ASCII-character-except-‘\’-‘'’-‘"’
                  | ‘\’〈escaped-char〉*/
  lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
    DefaultCharacter.build.lift(noneOf('\\', '\'', '\"'), "" #> false) <\>
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
    EscapedCharacter.build.lift(
      satisfy(EscapedCharacter.escapableCharacters.contains(_))
    )

  /*〈array-liter〉::= ‘[’ (〈expr〉(‘,’〈expr〉)* )?  ‘]’ */
  lazy val arrayLiterParser: Parsley[ArrayLiter] = ArrayLiter.build.lift(
    "[" *> option(
      expressionParser <~> combinator.many("," *> expressionParser)
    ) <* "]"
  )

  /*〈pair-liter〉::=  ‘null’ */
  lazy val pairLiterParser: Parsley[PairLiter] =
    PairLiter.build.lift("null")

  /* 〈comment〉::=  ‘#’ (any-character-except-EOL)*〈EOL〉 */
  lazy val commentParser: Parsley[Comment] =
    Comment.build.lift('#' *> combinator.manyUntil(anyChar, "\n"))

  def main(args: Array[String]): Unit = {
    // Testing the parser on some example inputs
    println("Hello World!")
    println(expressionParser.runParser("5+5")) // Should succeed
    println(expressionParser.runParser("152*55/22!=0")) // Should succeed
    println(integerLiterParser.runParser("5")) // Should succeed
    println(integerLiterParser.runParser("a")) // Should fail
  }
}
