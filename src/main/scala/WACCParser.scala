import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.functions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.statements._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.Parsley._
import parsley.character.{alphaNum, letter, noneOf, satisfy}
import parsley.combinator.{manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._
import parsley.{Parsley, combinator}

object WACCParser {

  /* 〈program〉::=  ‘begin’〈func〉*〈stat〉‘end’ */
  lazy val programParser: Parsley[Program] =
    (("begin" *> combinator.many(
      functionParser
    )) <~> (statementParser <* "end")).map {
      case (functions: List[Function], body: Statement) =>
        new Program(functions, body)
    }

  /* 〈func〉::=〈type〉 〈ident〉‘(’〈param-list〉?  ‘)’ ‘is’〈stat〉‘end’ */
  lazy val functionParser: Parsley[Function] = (
    typeParser <~>
      identifierParser <~>
      ('(' *> option(parameterListParser) <* ')') <~>
      ("is" *> statementParser <* "end")
  ).map { case (((functionType, functionName), optionParameters), body) =>
    new Function(functionType, functionName, optionParameters, body)
  }

  /* 〈param-list〉::=〈param〉( ‘,’〈param〉)* */
  lazy val parameterListParser: Parsley[ParameterList] =
    (parameterParser <~> combinator.many(',' *> parameterParser)).map {
      case (parameter: Parameter, parameters: List[Parameter]) =>
        new ParameterList(parameter :: parameters)
    }

  /* 〈param〉::=〈type〉 〈ident〉 */
  lazy val parameterParser: Parsley[Parameter] =
    (typeParser <~> identifierParser).map {
      case (parameterType, parameterName) =>
        new Parameter(parameterType, parameterName)
    }

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
    precedence.apply("skip").map(_ => new SkipStatement())
      <\> (typeParser <~> identifierParser <~> ("=" *> assignmentRightParser))
        .map { case ((identifierType, identifierName), assignmentRight) =>
          new IdentifierDeclaration(
            identifierType,
            identifierName,
            assignmentRight
          )
        }
      <\> (assignmentLeftParser <~> ('=' *> assignmentRightParser)).map {
        case (assignmentLeft, assignmentRight) =>
          new Assignment(assignmentLeft, assignmentRight)
      }
      <\> ("read" *> assignmentLeftParser).map(new Read(_))
      <\> ("free" *> expressionParser).map(new Free(_))
      <\> ("return" *> expressionParser).map(new Return(_))
      <\> ("exit" *> expressionParser).map(new Exit(_))
      <\> ("print" *> expressionParser).map(new Print(_))
      <\> ("println" *> expressionParser).map(new Println(_))
      <\> (("if" *> expressionParser) <~> ("then" *> statementParser) <~> ("else" *> statementParser <* "fi"))
        .map { case ((condition, statement1), statement2) =>
          new If(condition, statement1, statement2)
        }
      <\> (("while" *> expressionParser) <~> ("do" *> statementParser <* "done"))
        .map { case (condition, statement) =>
          new While(condition, statement)
        }
      <\> ("begin" *> statementParser <* "end").map(new BeginEnd(_)),
    Ops(InfixL)(";" #> (new StatementSequence(_, _)))
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
    (("newpair(" *> expressionParser) <~> (',' *> expressionParser <* ')'))
      .map { case (expr1: Expression, expr2: Expression) =>
        new NewPair(expr1, expr2)
      }

  lazy val functionCallParser: Parsley[FunctionCall] =
    (("call" *> identifierParser) <~> ('(' *> option(
      argumentListParser
    ) <* ')')).map {
      case (identifier: Identifier, optionArguments: Option[ArgumentList]) =>
        new FunctionCall(identifier, optionArguments)
    }

  /*〈arg-list〉::=〈expr〉(‘,’〈expr〉)* */
  lazy val argumentListParser: Parsley[ArgumentList] =
    (expressionParser <~> combinator.many(',' *> expressionParser)).map {
      case (expression: Expression, expressions: List[Expression]) =>
        new ArgumentList(expression :: expressions)
    }

  /* 〈pair-elem〉::= ‘fst’〈expr〉
                    |‘snd’〈expr〉 */
  lazy val pairElementParser: Parsley[PairElement] =
    (("fst" *> expressionParser) <~> ("snd" *> expressionParser)).map {
      case (expr1, expr2) => new PairElement(expr1, expr2)
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
    ("int" <\> "bool" <\> "char" <\> "string").map(BaseType(_))

  /*〈array-type〉::=〈type〉‘[’ ‘]’ */
  lazy val arrayTypeParser: Parsley[ArrayType] =
    (typeParser <* '[' <* ']').map(new ArrayType(_))

  /*〈pair-type〉::=  ‘pair’ ‘(’〈pair-elem-type〉‘,’〈pair-elem-type〉‘)’ */
  lazy val pairTypeParser: Parsley[PairType] =
    (("pair(" *> pairElementTypeParser) <~> ("," *> pairElementTypeParser <* ")"))
      .map { case (type1, type2) =>
        new PairType(type1, type2)
      }

  /* 〈pair-elem-type〉::=〈base-type〉
                       | 〈array-type〉
                       | ‘pair’ */
  lazy val pairElementTypeParser: Parsley[PairElementType] =
    baseTypeParser <\> arrayTypeParser <\> precedence
      .apply("pair")
      .map(_ => new Pair())

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
    (unaryOperatorParser <~> expressionParser).map {
      case (operator, expression) =>
        new UnaryOperatorApplication(operator, expression)
    }

  lazy val binaryFunctionGenerator
      : String => (Expression, Expression) => BinaryOperatorApplication =
    (operator: String) =>
      (expr1: Expression, expr2: Expression) =>
        new BinaryOperatorApplication(expr1, BinaryOperator(operator), expr2)

  /* 〈unary-oper〉::= ‘!’
                    | ‘-’
                    | ‘len’
                    | ‘ord’
                    | ‘chr’ */
  lazy val unaryOperatorParser: Parsley[UnaryOperator] =
    ("!" <\> "-" <\> "len" <\> "ord" <\> "chr").map(UnaryOperator(_))

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
    ("*" <\> "/" <\> "%" <\> "+" <\> "-" <\> ">"
      <\> ">=" <\> "<" <\> "<=" <\> "==" <\> "!=" <\> "&&" <\> "||")
      .map(BinaryOperator(_))

  /*〈ident〉::=  ( ‘’|‘a’-‘z’|‘A’-‘Z’ ) ( ‘’|‘a’-‘z’|‘A’-‘Z’|‘0’-‘9’ )* */
  lazy val identifierParser: Parsley[Identifier] =
    (('_' <\> letter) <~> combinator.many('_' <\> alphaNum)).map {
      case (letter, letters) =>
        new Identifier((letter :: letters).mkString(""))
    }

  /*〈array-elem〉::=〈ident〉(‘[’〈expr〉‘]’)+ */
  lazy val arrayElementParser: Parsley[ArrayElement] =
    (identifierParser <~> manyN(1, "[" *> expressionParser <* "]")).map {
      case (identifier, expressions) =>
        new ArrayElement(identifier, expressions)
    }

  /* 〈int-liter〉::=〈int-sign〉?〈digit〉+ */
  lazy val integerLiterParser: Parsley[IntegerLiter] =
    (option(integerSignParser) <~> manyN(1, digitParser)).map {
      case (sign, digits) => new IntegerLiter(sign, digits)
    }

  /*〈digit〉::=  (‘0’-‘9’) */
  lazy val digitParser: Parsley[Digit] =
    ('0' <\> '1' <\> '2' <\> '3' <\> '4' <\> '5' <\> '6' <\> '7' <\> '8' <\> '9')
      .map(new Digit(_))

  /*〈int-sign〉::=  ‘+’|‘-’ */
  lazy val integerSignParser: Parsley[IntegerSign] =
    ('+' <\> '-').map(sign => new IntegerSign(sign))

  /*〈bool-liter〉::=  ‘true’|‘false’ */
  lazy val booleanLiterParser: Parsley[BooleanLiter] =
    ("true" <\> "false").map {
      case "true"  => new BooleanLiter(true)
      case "false" => new BooleanLiter(false)
    }

  /*〈char-liter〉::=  ‘'’〈character〉‘'’ */
  lazy val characterLiterParser: Parsley[CharacterLiter] =
    ("\'" *> defaultCharacterParser <* "\'").map(character =>
      new CharacterLiter(character.char)
    )

  /*〈str-liter〉::=  ‘"’〈character〉* ‘"’ */
  lazy val stringLiterParser: Parsley[StringLiter] =
    ("\"" *> combinator.many(defaultCharacterParser) <* "\"").map(characters =>
      new StringLiter(characters.mkString(""))
    )

  /*〈character〉::= any-ASCII-character-except-‘\’-‘'’-‘"’
                  | ‘\’〈escaped-char〉*/
  lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
    (noneOf('\\', '\'', '\"')).map(
      new DefaultCharacter(_, false)
    ) <\> ('\\' *> escapedCharParser).map(escapedCharacter =>
      new DefaultCharacter(escapedCharacter.char, true)
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
    satisfy(EscapedCharacter.escapableCharacters.contains(_))
      .map(new EscapedCharacter(_))

  /*〈array-liter〉::= ‘[’ (〈expr〉(‘,’〈expr〉)* )?  ‘]’ */
  lazy val arrayLiterParser: Parsley[ArrayLiter] = ("[" *> option(
    expressionParser <~> combinator.many("," *> expressionParser)
  ) <* "]").map {
    case None => new ArrayLiter(List())
    case Some((expression: Expression, expressions: List[Expression])) =>
      new ArrayLiter(expression :: expressions)
  }

  /*〈pair-liter〉::=  ‘null’ */
  lazy val pairLiterParser: Parsley[PairLiter] =
    precedence.apply("null").map(_ => new PairLiter())

  /* 〈comment〉::=  ‘#’ (any-character-except-EOL)*〈EOL〉 */
  lazy val commentParser: Parsley[Comment] =
    ('#' *> combinator.many(noneOf('\n')))
      .map(comment => new Comment(comment.mkString("")))

  def main(args: Array[String]): Unit = {
    // Testing the parser on some example inputs
    println("Hello World!")
    println(expressionParser.runParser("5+5")) // Should succeed
    println(expressionParser.runParser("152*55/22!=0")) // Should succeed
    println(integerLiterParser.runParser("5")) // Should succeed
    println(integerLiterParser.runParser("a")) // Should fail
  }
}
